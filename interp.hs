-- Interpreter for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

module Interp where
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BSC
import qualified Network.Socket as SO
import Data.List (intercalate)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, get, put)
import System.IO (Handle, hPutStr, hGetLine, hFlush, hClose, hIsEOF, openBinaryFile, IOMode(..), stdout, stdin)
import System.Directory (doesFileExist)
import System.FilePath (FilePath, splitExtension)
import AST
import Parser (parseProgram)

-- for Show
newtype BIF = BIF (Value -> InterpState Value)
instance Show BIF where show _ = "<built-in>"
instance Eq BIF where a == b = False

data Value = IntV Integer
		   | StrV String
		   | UnitV
		   | BoolV Bool
		   | StreamV Int
		   | TupleV [Value]
		   | ListV [Value]
		   | DictV (M.Map Value Value)
		   | Builtin BIF
		   | FnV Env [(Pattern, AST)] -- closure pattern->body bindings
		   deriving (Eq)

type Env = [M.Map String Value] -- lexical environment (linked list)
type InterpState = StateT ([Handle], Env) IO -- interpreter state (open handles, global env)

emptyEnv = [M.empty]

-- look up a binding from the bottom up
lookup :: Env -> String -> Maybe Value
lookup [] _ = Nothing
lookup (env:xs) name =
	case M.lookup name env of
		Nothing -> lookup xs name
		Just x -> Just x

-- bind in the local environment
bind :: Env -> String -> Value -> Env
bind (env:xs) name value = (M.insert name value env):xs

instance Show Value where
	show (IntV i) = show i
	show (StrV s) = show s
	show (BoolV b) = show b
	show (TupleV v) = "(" ++ intercalate "," (map show v) ++ ")"
	show (ListV v) = show v
	show (DictV d) = "<dict " ++ show d ++ ">"
	show (FnV _ _) = "<fn>"
	show (StreamV _) = "<stream>"
	show (Builtin _) = "<built-in>"
	show UnitV = "()"

-- value operators
(IntV l) +$ (IntV r) = IntV (l + r)
(StrV l) +$ (StrV r) = StrV (l ++ r)
(ListV l) +$ (ListV r) = ListV (l ++ r)
l +$ r = error $ "cannot + " ++ show l ++ " and " ++ show r

(IntV l) -$ (IntV r) = IntV (l - r)
l -$ r = error $ "cannot - " ++ show l ++ " and " ++ show r

(IntV l) *$ (IntV r) = IntV (l * r)
l *$ r = error $ "cannot * " ++ show l ++ " and " ++ show r

(IntV l) /$ (IntV r) = IntV (l `div` r)
l /$ r = error $ "cannot / " ++ show l ++ " and " ++ show r

(IntV l) <$ (IntV r) = BoolV (l < r)
l <$ r = error $ "cannot < " ++ show l ++ " and " ++ show r

(IntV l) >$ (IntV r) = BoolV (l > r)
l >$ r = error $ "cannot > " ++ show l ++ " and " ++ show r

l ==$ r = BoolV (l == r)
l !=$ r = BoolV (l /= r)

-- some built-in functions

_fputstr (TupleV [StreamV h, StrV str]) = do
	(handles,_) <- get
	let handle = handles !! h
	io <- lift $ hPutStr handle str >> hFlush handle
	return UnitV

_fgetline (StreamV h) = do
	(handles,_) <- get
	let handle = handles !! h
	str <- lift $ hGetLine handle
	if last str == '\r' then -- remove trailing CR
		return . StrV $ init str
	else return $ StrV str

_fread (TupleV [StreamV h, IntV n]) = do
	(handles,_) <- get
	let handle = handles !! h
	str <- lift $ BSC.hGet handle (fromIntegral n :: Int)
	return . StrV $ BSC.unpack str

_fopen (TupleV [StrV path, StrV mode]) = do
	(handles,env) <- get
	let mode' = case mode of
		"r" -> ReadMode
		"w" -> WriteMode
		"rw" -> ReadWriteMode
	handle <- lift $ openBinaryFile path mode'
	put (handles ++ [handle], env)
	return . StreamV $ length handles

_feof (StreamV h) = do
	(handles,_) <- get
	let handle = handles !! h
	isEof <- lift $ hIsEOF handle
	return $ BoolV isEof

_fclose handle@(StreamV h) = do
	(handles,_) <- get
	let handle = handles !! h
	lift $ hClose handle
	return UnitV

_sockopen (TupleV [StrV host, IntV port]) = do
	(handles,env) <- get
	handle <- lift $ SO.withSocketsDo $ do
		addr:_ <- SO.getAddrInfo Nothing (Just host) (Just $ show port)
		sock <- SO.socket (SO.addrFamily addr) SO.Stream SO.defaultProtocol
		SO.connect sock (SO.addrAddress addr)
		handle <- SO.socketToHandle sock ReadWriteMode
		return handle
	put (handles ++ [handle], env)
	return . StreamV $ length handles

_putstr str@(StrV _) = _fputstr $ TupleV [StreamV 0, str]
_getline UnitV = _fgetline (StreamV 1)

_print v = _putstr $ StrV $ show v ++ "\n"
_repr v = return . StrV $ show v

_itos (IntV i) = return $ StrV $ show i
_itos v = error $ "itos: not an int: " ++ show v

_loop args@(TupleV [fn@(FnV _ _), arg]) = do
	v <- apply fn arg
	if v /= BoolV False then
		_loop $ TupleV [fn, v]
	else return arg

-- import a module name as a module
_Import (StrV modname) = do
	(h,env) <- get -- save current state
	put initialState
	(path,modname) <- lift $ findModule modname -- find the module file
	evalFile path -- evaluate the module file
	(_,[modenv]) <- get -- get the module env
	let (_, [initialEnv]) = initialState
	let modenv' = M.difference modenv initialEnv -- subtract prelude stuff
	-- convert String to StrV in env keys
	let modenv'' = map (\(k,v) -> (StrV k, v)) $ M.toAscList modenv'
	let mod = DictV (M.fromAscList modenv'') -- package module into a dict
	let env' = bind env modname mod -- bind it
	put (h,env') -- restore state
	return mod -- return module value
	
	where
		findModule :: FilePath -> IO (FilePath, String)
		findModule modname = do
			let path = modname ++ ".lamb"
			exists <- doesFileExist path
			if exists then
				return (path, fst $ splitExtension path)
			else error $ "module " ++ modname ++ " couldn't be found"

initialState = ([stdout, stdin],
					[M.fromList [("id", FnV emptyEnv [(VarP "x", Var "x")]),
								("loop", Builtin $ BIF _loop),
						   		("repr", Builtin $ BIF _repr),
						   		("stdout", StreamV 0),
						   		("stdin", StreamV 1),
						   		("print", Builtin $ BIF _print),
						   		("putstr", Builtin $ BIF _putstr),
						   		("putstrln", Builtin $ BIF (\x -> _putstr $ x +$ StrV "\n")),
						   		("getline", Builtin $ BIF _getline),
						   		("fgetline", Builtin $ BIF _fgetline),
						   		("fputstr", Builtin $ BIF _fputstr),
						   		("fread", Builtin $ BIF _fread),
						   		("feof", Builtin $ BIF _feof),
						   		("fclose", Builtin $ BIF _fclose),
						   		("fopen", Builtin $ BIF _fopen),
						   		("sockopen", Builtin $ BIF _sockopen),
						   		("itos", Builtin $ BIF _itos),
						   		("import", Builtin $ BIF _Import)]])

eval :: AST -> InterpState Value

eval (IntConst i) = return $ IntV i
eval (StrConst s) = return $ StrV s
eval (BoolConst b) = return $ BoolV b

eval UnitConst = return UnitV

eval (Block body) = foldr1 (>>) $ map eval body

eval (Cons a b) = do
	a' <- eval a
	b' <- eval b
	case b' of
		ListV v' -> return $ ListV $ a':v'
		_ -> error "cons: RHS must be a list"

eval (ListConst v) =
	mapM eval v >>= \xs ->
		return $ ListV xs

eval (TupleConst v) = mapM eval v >>= return . TupleV

eval (IfExpr c t e) = eval c >>= \cond ->
	case cond of
		BoolV True -> eval t
		BoolV False -> eval e
		_ -> error "if: condition must be a boolean"

eval (Var var) = get >>= \(_,env) ->
  case lookup env var of
    Just v -> return v
    Nothing -> error $ "unbound variable " ++ var

eval (Defun name fn) = do
	(s,env) <- get
	case lookup env name of
            Nothing -> -- bind new fn
                    eval fn >>= \fn' ->
                    	put (s, bind env name fn') >> return fn'
            Just oldfn -> -- add pattern to old fn
                    let FnV cls oldpats = oldfn
                        Lambda [(pat, body)] = fn
                        newfn = FnV cls (oldpats ++ [(pat, body)]) in
                        put (s, bind env name newfn) >> return newfn

eval (Def pat v') = do
	v <- eval v'
	(s,locals:xs) <- get
	case patternBindings pat v of
		Nothing -> error $ "pattern binding doesn't satisfy: " ++ show v ++ " with " ++ show pat
		Just bindings ->
			put (s, (M.union bindings locals):xs) >> -- update our local bindings
			return v

eval (Lambda pats) =
	get >>= \(_,env) ->
		if length env == 1 then -- if in global env just use [], denoting the current global scope
			return $ FnV [] pats
		else return $ FnV env pats

eval (Add l r) = do { l <- eval l; r <- eval r; return $ l +$ r }
eval (Sub l r) = do { l <- eval l; r <- eval r; return $ l -$ r }
eval (Mul l r) = do { l <- eval l; r <- eval r; return $ l *$ r }
eval (Div l r) = do { l <- eval l; r <- eval r; return $ l /$ r }

eval (Equals l r) = do { l <- eval l; r <- eval r; return $ l ==$ r }
eval (NotEquals l r) = do { l <- eval l; r <- eval r; return $ l !=$ r }
eval (LessThan l r) = do { l <- eval l; r <- eval r; return $ l <$ r }
eval (GreaterThan l r) = do { l <- eval l; r <- eval r; return $ l >$ r }

eval (Call lhs arg) = do
	(h,env) <- get
	v <- eval lhs
	case v of
	  fn@(FnV cls _) -> do
	  	arg' <- eval arg
	  	let cls' = if cls == [] then [last env] else cls -- if [], use current global env
	  	put (h,cls') -- enter closure env
	  	v <- apply fn arg'
	  	put (h,env) -- restore env
	  	return v
	  fn@(Builtin _) -> eval arg >>= apply fn
	  _ -> error $ "call: " ++ show v ++ " is not a function"

eval x = error $ "eval: unhandled: " ++ show x

patternBindings :: Pattern -> Value -> Maybe (M.Map String Value)
patternBindings (VarP n) v = Just $ M.fromList [(n, v)]

patternBindings (IntP n) (IntV v)
	| v == n = Just M.empty
	| otherwise = Nothing
patternBindings (IntP n) _ = Nothing

patternBindings UnitP UnitV = Just M.empty
patternBindings UnitP _ = Nothing

patternBindings (StrP x) (StrV y)
	| x == y = Just M.empty
	| otherwise = Nothing
patternBindings (StrP _) _ = Nothing

-- cons on strings
patternBindings (ConsP x (ListP [])) (StrV (y:[])) = patternBindings x (StrV [y])
-- "xy":xs pattern
patternBindings (ConsP (StrP xp) xsp) (StrV str) =
	let len = length xp in
	if take len str == xp then -- matches
		patternBindings xsp $ StrV (drop len str) -- match the rest of the string
	else Nothing -- no match
patternBindings (ConsP xp xsp) (StrV (x:xs)) =
	do
		xe <- patternBindings xp (StrV [x])
		xse <- patternBindings xsp $ StrV xs
		Just $ M.union xe xse

-- cons on lists
patternBindings (ConsP x (ListP [])) (ListV (y:[])) = patternBindings x y
patternBindings (ConsP xp xsp) (ListV (x:xs)) =
	do
		xe <- patternBindings xp x
		xse <- patternBindings xsp $ ListV xs
		Just $ M.union xe xse
patternBindings (ConsP _ _) _ = Nothing

-- lists
patternBindings (ListP []) (ListV (x:_)) = Nothing -- not enough patterns
patternBindings (ListP (_:_)) (ListV []) = Nothing -- not enough values
patternBindings (ListP []) (ListV []) = Just M.empty -- base case
patternBindings (ListP (x:xs)) (ListV (y:ys)) =
	do
		env <- patternBindings x y
		env' <- patternBindings (ListP xs) (ListV ys)
		Just $ M.union env' env
patternBindings (ListP _) _ = Nothing -- not a list

-- tuples
patternBindings (TupleP []) (TupleV (x:_)) = Nothing -- not enough patterns
patternBindings (TupleP (_:_)) (TupleV []) = Nothing -- not enough values
patternBindings (TupleP []) (TupleV []) = Just M.empty -- base case
patternBindings (TupleP (x:xs)) (TupleV (y:ys)) =
	do
		env <- patternBindings x y
		env' <- patternBindings (TupleP xs) (TupleV ys)
		Just $ M.union env' env
patternBindings (TupleP _) _ = Nothing -- not a tuple

-- applies a function
apply :: Value -> Value -> InterpState Value
apply (FnV _ pats) arg =
	apply' pats
	where
		apply' [] = error $ "argument " ++ show arg ++ " doesn't satisfy any patterns"
		apply' ((pat, body):xs) =
			case patternBindings pat arg of
				Just bindings -> -- satisfies
					do
						(s,env) <- get
						let newenv = bindings:env
						put (s, newenv)
						eval body
				Nothing -> -- doesn't satisfy this pattern
					apply' xs

apply (Builtin (BIF fn)) arg = fn arg

-- some helper programs for evaluation

evalProgram :: [AST] -> InterpState Value
evalProgram nodes = foldr1 (>>) $ map eval nodes

evalString :: String -> InterpState Value
evalString program =
	case parseProgram program of
		Left err -> error $ show err
		Right prg -> evalProgram prg

isLiterate :: FilePath -> Bool
isLiterate path = snd (splitExtension path) == ".lilamb"

-- Takes the lines of a literate program and returns the lines for a new executable program
-- from lines beginning with four spaces.
parseLiterate :: [String] -> [String]
parseLiterate lns = [drop 4 line | line <- lns, take 4 line == "    "]

evalFile :: FilePath -> InterpState Value
evalFile path = do
	contents <- lift $ if path == "-" then getContents else readFile path
	if isLiterate path then
		evalString . unlines . parseLiterate . lines $ contents
	else evalString contents

evalFileV :: FilePath -> IO Value
evalFileV path = evalStateT (evalFile path) initialState