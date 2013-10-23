-- Interpreter for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

module Interp where
import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.List (intercalate)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, get, put)
import System.IO (Handle, hPutStr, hGetLine, hFlush, stdout, stdin)
import System.IO.Unsafe (unsafePerformIO)
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
	show (TupleV v) = "(" ++ intercalate "," (map show v) ++ ")"
	show (ListV v) = show v
	show (FnV _ _) = "<fn>"
	show (StreamV _) = "<stream>"
	show (Builtin _) = "<built-in>"
	show UnitV = "()"

-- value operators
(IntV l) +$ (IntV r) = IntV (l + r)
(StrV l) +$ (StrV r) = StrV (l ++ r)
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

_putstr (StrV str) = do
	(handles,_) <- get
	let stdout_s = head handles
	io <- lift $ hPutStr stdout_s str >> hFlush stdout_s
	return UnitV

_print v = _putstr $ StrV $ show v ++ "\n"

_getline UnitV = do
	(handles,_) <- get
	let stdin_s = handles !! 1
	str <- lift $ hGetLine stdin_s
	return $ StrV str

_itos (IntV i) = return $ StrV $ show i
_itos v = error $ "itos: not an int: " ++ show v

initialState = ([stdout, stdin],
					[M.fromList [("id", FnV emptyEnv [(VarP "x", Var "x")]),
						   		("stdout", StreamV 0),
						   		("putstr", Builtin $ BIF _putstr),
						   		("putstrln", Builtin $ BIF (\x -> _putstr $ x +$ StrV "\n")),
						   		("print", Builtin $ BIF _print),
						   		("itos", Builtin $ BIF _itos),
						   		("getline", Builtin $ BIF _getline)]])

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

eval (Def name v') = do
	v <- eval v'
	(s,env) <- get
	put (s, bind env name v)
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

eval (Call name arg) = get >>= \(h,env) ->
	case lookup env name of
	  Just fn@(FnV cls _) -> do
	  	arg' <- eval arg
	  	let cls' = if cls == [] then [last env] else cls -- if [], use current global env
	  	put (h,cls') -- enter closure env
	  	v <- apply fn arg'
	  	put (h,env) -- restore env
	  	return v
	  Just fn@(Builtin _) -> eval arg >>= apply fn
	  Nothing -> error $ "call: name " ++ name ++ " doesn't exist or is not a function"

eval x = error $ "eval: unhandled: " ++ show x

patternBindings :: Pattern -> Value -> Maybe (M.Map String Value)
patternBindings (VarP n) v = Just $ M.fromList [(n, v)]

patternBindings (IntP n) (IntV v)
	| v == n = Just M.empty
	| otherwise = Nothing
patternBindings (IntP n) _ = Nothing

patternBindings UnitP UnitV = Just M.empty
patternBindings UnitP _ = Nothing

patternBindings (ConsP x (ListP [])) (ListV (y:[])) = patternBindings x y
patternBindings (ConsP xp xsp) (ListV (x:xs)) =
	do
		xe <- patternBindings xp x
		xse <- patternBindings xsp $ ListV xs
		Just $ M.union xe xse
patternBindings (ConsP _ _) _ = Nothing

-- lists
patternBindings (ListP []) (ListV (x:xs)) = Nothing -- not enough patterns
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

evalProgram :: [AST] -> IO Value -- fold the state from each node and return the result
evalProgram nodes = evalStateT (foldr1 (>>) $ map eval nodes) initialState

evalString :: String -> IO Value
evalString program =
	case parseProgram program of
		Left err -> error $ show err
		Right prg -> evalProgram prg
