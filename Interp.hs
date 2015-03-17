-- Interpreter for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

module Interp where
import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BSC
import qualified Network.Socket as SO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (intercalate, foldl1')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))
import Control.Exception (try, SomeException)
import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, get, put)
import System.IO (Handle, hPutStr, hGetLine, hClose, hIsEOF, hSetBuffering,
		hSetBinaryMode, openBinaryFile, IOMode(..), BufferMode(NoBuffering), stdout, stdin)
import System.Directory (doesFileExist)
import System.FilePath (FilePath, splitExtension, takeBaseName, takeDirectory, (</>))
import System.Environment (getExecutablePath)
import AST
import Parser (parseProgram)

-- for Show
newtype BIF = BIF (Value -> InterpState Value)
instance Show BIF where show _ = "<built-in>"
instance Eq BIF where a == b = False
instance Ord BIF where compare a b = if a == b then EQ else LT

data Value = IntV Integer
		   | StrV T.Text
		   | BoolV Bool
		   | StreamV Handle
		   | TupleV [Value]
		   | ListV [Value]
		   | DictV (M.Map Value Value)
		   | RefV (TVar Value)
		   | Thread ThreadId
		   | Builtin BIF
		   | FnV Env [(Pattern, AST)] -- closure pattern->body bindings
		   deriving (Eq)

instance Ord Value where
	compare (IntV a) (IntV b) = compare a b
	compare (StrV a) (StrV b) = compare a b
	compare (BoolV a) (BoolV b) = compare a b
	compare (TupleV a) (TupleV b) = compare a b
	compare (ListV a) (ListV b) = compare a b
	compare (StreamV a) (StreamV b) = if a == b then EQ else LT
	compare (Builtin a) (Builtin b) = compare a b
	compare (FnV a b) (FnV x y) = if a == x && b == y then EQ else LT
	compare (DictV a) (DictV b) = compare a b
	compare _ _ = error "compare: not valid"

type Env = [M.Map T.Text Value] -- lexical environment (linked list)
type InterpState = StateT Env IO -- interpreter state (open handles, global env)

type StrDict = M.Map T.Text Value
type ValueDict = M.Map Value Value

emptyEnv = [M.empty]
unitv = TupleV []

-- look up a binding from the bottom up
lookup :: Env -> T.Text -> Maybe Value
lookup [] _ = Nothing
lookup (env:xs) name = maybe (lookup xs name) Just (M.lookup name env)

-- bind in the local environment
bind :: Env -> T.Text -> Value -> Env
bind (env:xs) name value = (M.insert name value env):xs

instance Show Value where
	show (IntV i) = show i
	show (StrV s) = show s
	show (BoolV b) = show b
	show (TupleV []) = "(,)"
	show (TupleV v) = "(" ++ intercalate "," (map show v) ++ ")"
	show (ListV v) = show v
	show (DictV d) = "<dict " ++ show d ++ ">"
	show (FnV _ _) = "<fn>"
	show (StreamV _) = "<stream>"
	show (Builtin _) = "<built-in>"
	show (RefV _) = "<ref>"
	show (Thread t) = "<thread " ++ show t ++ ">"

-- value operators
(IntV l) +$ (IntV r) = IntV (l + r)
(StrV l) +$ (StrV r) = StrV (l `T.append` r)
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

toDict :: StrDict -> Value
toDict m = DictV (M.mapKeys StrV m) -- wrap keys in StrV

fromDict :: ValueDict -> StrDict
fromDict m = M.mapKeys (\(StrV k) -> k) m -- unwrap keys

-- some built-in functions

_fputstr (TupleV [StreamV handle, StrV str]) =
	liftIO $ TIO.hPutStr handle str >> return unitv

_fgetline (StreamV handle) = do
	str <- liftIO $ TIO.hGetLine handle
	if T.last str == '\r' then -- remove trailing CR
		return . StrV $ T.init str
	else return $ StrV str

_freadbytes (TupleV [StreamV handle, IntV n]) = do
	liftIO $ StrV . T.take (fromIntegral n) <$> TIO.hGetContents handle

_freadcontents (StreamV handle) = do
	liftIO $ StrV <$> TIO.hGetContents handle

_fopen (TupleV [StrV path, StrV mode]) = do
	let mode' = case T.unpack mode of
		"r" -> ReadMode
		"w" -> WriteMode
		"rw" -> ReadWriteMode
	StreamV <$> liftIO (openBinaryFile (T.unpack path) mode')

_feof (StreamV handle) = do
	BoolV <$> liftIO (hIsEOF handle)

_fclose (StreamV handle) = do
	liftIO (hClose handle) >> return unitv

_sockopen (TupleV [StrV host, IntV port]) = do
	liftIO $ SO.withSocketsDo $ do
		addr:_ <- SO.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
		sock <- SO.socket (SO.addrFamily addr) SO.Stream SO.defaultProtocol
		SO.connect sock (SO.addrAddress addr)
		handle <- SO.socketToHandle sock ReadWriteMode
		hSetBuffering handle NoBuffering
		return $ StreamV handle

_putstr str@(StrV _) = _fputstr $ TupleV [StreamV stdout, str]
_putbytes str@(StrV _) = _fputstr $ TupleV [StreamV stdout, str]
_getline (TupleV []) = _fgetline (StreamV stdin)

_print v = _putbytes $ StrV $ T.pack (show v) `T.snoc` '\n'
_repr v = return . StrV $ T.pack $ show v

_itos (IntV i) = return $ StrV $ T.pack $ show i
_itos v = error $ "itos: not an int: " ++ show v

_stoi (StrV s) = return $ IntV $ read $ T.unpack s
_stoi v = error $ "stoi: not a string: " ++ show v

_ref v = RefV <$> liftIO (newTVarIO v)

_readRef (RefV r) = liftIO $ atomically $ readTVar r

_setRef (TupleV [RefV r, v]) =
	liftIO (atomically $  writeTVar r v) >> return v

_time (TupleV []) = fmap IntV $ liftIO $ round <$> getPOSIXTime

_sleep (IntV milliseconds) = liftIO (threadDelay (fromInteger $ 1000 * milliseconds)) >> return unitv

_loop args@(TupleV [fn@(FnV _ _), arg]) = do
	v <- apply fn arg
	if v /= BoolV False then
		_loop $ TupleV [fn, v]
	else return arg

_eval (TupleV [StrV code, DictV env]) = do
	let trySome :: IO a -> IO (Either SomeException a)
	    trySome = try
	    state = [fromDict env]
	ret <- liftIO . trySome $ evalStateT (evalString code) state
	case ret of
		Left err -> return $ TupleV [StrV (T.pack "err"), StrV $ T.pack (show err)]
		Right v -> return v

_eval (TupleV [code@(StrV _), (ListV env)]) =
	let env' = map (\(TupleV [k,v]) -> (k,v)) env in
	_eval (TupleV [code, DictV $ M.fromList env'])

_eval _ = error "eval: invalid args (want code and environment)"

_thread f@FnV{} = do
	state <- get
	fmap Thread $ liftIO $ forkIO $ (evalStateT (apply f unitv) state >> return ())
_thread _ = error "thread!: need a function"

_kill (Thread thread) = liftIO (killThread thread) >> return unitv
_kill _ = error "kill!: need a thread"

-- returns a dictionary of a new environment with only the standard 
-- default-imported functions
_newStdEnv (TupleV []) = do
	let [stdEnv] = initialState
	return $ toDict stdEnv

_globals (TupleV []) = do
	env <- get
	return $ toDict (last env)

_locals (TupleV []) = do
	locals:_ <- get
	return $ toDict locals

-- import a module name as a module
_Import (StrV modname) = do
	env <- get -- save current state
	put initialState
	(path,modname) <- liftIO $ findModule $ T.unpack modname -- find the module file
	evalFile path -- evaluate the module file
	[modenv] <- get -- get the module env
	let [initialEnv] = initialState
	--let modenv' = M.difference modenv initialEnv -- subtract prelude stuff
	let mod = toDict modenv
	let env' = bind env (T.pack modname) mod -- bind it
	put env' -- restore state
	return mod -- return module value
	
	where
		findModule :: FilePath -> IO (FilePath, String)
		findModule modname = do
			execPath <- fmap takeDirectory getExecutablePath
			findModuleIn [".", execPath </> "mods"] -- search paths for modules
			where
				findModuleIn [] = error $ "module " ++ modname ++ " couldn't be found"
				findModuleIn (dir:xs) = do
					let path = dir </> modname ++ ".lamb"
					exists <- doesFileExist path
					if exists then return (path, takeBaseName path)
					else findModuleIn xs

bif = Builtin . BIF
initialState = [M.fromList $ map (\(k,v) -> (T.pack k, v)) $ [
                            ("id", FnV emptyEnv [(VarP (T.pack "x"), Var (T.pack "x"))]),
                            ("loop", bif _loop),
                            ("ref!", bif _ref),
                            ("readRef!", bif _readRef),
                            ("setRef!", bif _setRef),
                            ("time!", bif _time),
                            ("sleep!", bif _sleep),
                            ("repr", bif _repr),
                            ("stdout", StreamV stdout),
                            ("stdin", StreamV stdin),
                            ("print", bif _print),
                            ("putstr", bif _putstr),
                            ("putstrln", bif (\x -> _putstr $ x +$ StrV (T.singleton '\n'))),
                            ("getline", bif _getline),
                            ("fgetline", bif _fgetline),
                            ("putbytes", bif _putbytes),
                            ("fputbytes", bif _fputstr),
                            ("fputstr", bif _fputstr),
                            ("freadbytes", bif _freadbytes),
                            ("freadcontents", bif _freadcontents),
                            ("feof", bif _feof),
                            ("fclose", bif _fclose),
                            ("fopen", bif _fopen),
                            ("sockopen", bif _sockopen),
                            ("itos", bif _itos),
                            ("stoi", bif _stoi),
                            ("globals", bif _globals),
                            ("locals", bif _locals),
                            ("newStdEnv", bif _newStdEnv),
                            ("thread!", bif _thread),
                            ("kill!", bif _kill),
                            ("eval", bif _eval),
                            ("import", bif _Import)]]

eval :: AST -> InterpState Value

eval (IntConst i) = return $ IntV i
eval (StrConst s) = return $ StrV s
eval (BoolConst b) = return $ BoolV b

eval (Block body) = foldr1 (>>) $ map eval body

eval (Cons a b) = do
	a' <- eval a
	b' <- eval b
	case b' of
		ListV v' -> return $ ListV $ a':v'
		StrV v' ->
			case a' of
				StrV c | T.length c == 1 -> return $ StrV $ T.cons (T.head c) v'
				_ -> error "cons: LHS must be a char"
		_ -> error "cons: RHS must be a list or string"

eval (ListConst v) = ListV <$> mapM eval v
eval (TupleConst v) = TupleV <$> mapM eval v

eval (IfExpr c t e) = eval c >>= \cond ->
	case cond of
		BoolV True -> eval t
		BoolV False -> eval e
		_ -> error "if: condition must be a boolean"

eval (Var var) = get >>= \env ->
	maybe (error $ "unbound variable " ++ T.unpack var) return (lookup env var)

eval (Defun name fn) = do
	env <- get
	case lookup env name of
            Nothing -> -- bind new fn
                    eval fn >>= \fn' ->
                    	put (bind env name fn') >> return fn'
            Just oldfn -> -- add pattern to old fn
                    let FnV cls oldpats = oldfn
                        Lambda [(pat, body)] = fn
                        newfn = FnV cls (oldpats ++ [(pat, body)]) in
                        put (bind env name newfn) >> return newfn

eval (Def pat v') = do
	v <- eval v'
	locals:xs <- get
	case patternBindings pat v of
		Nothing -> error $ "pattern binding doesn't satisfy: " ++ show v ++ " with " ++ show pat
		Just bindings -> do
			put $ (M.union bindings locals):xs -- update our local bindings
			return v

eval (Lambda pats) = do
	env <- get
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

eval (Access left (Var right)) = do
	lhs <- eval left
	case lhs of
		DictV dict ->
			case M.lookup (StrV right) dict of
				Just (FnV [] fn) -> -- use the module's global scope
					return $ FnV (mapToEnv dict) fn
				Just v -> return v
				Nothing -> return $ TupleV [StrV (T.pack "nothing")]
		_ -> error $ "op/: need a dict, got " ++ show lhs
	where
		mapToEnv :: M.Map Value Value -> Env
		mapToEnv m = [fromDict m]
eval (Access _ _) = error "op/: RHS must be an identifier"

eval (Call lhs arg) = do
	env <- get
	v <- eval lhs
	case v of
	  fn@(FnV cls _) -> do
	  	arg' <- eval arg
	  	let cls' = if cls == [] then [last env] else cls -- if [], use current global env
	  	put cls' -- enter closure env
	  	v <- apply fn arg'
	  	put env -- restore env
	  	return v
	  fn@(Builtin _) -> eval arg >>= apply fn
	  _ -> error $ "call: " ++ show v ++ " is not a function"

eval x = error $ "eval: unhandled: " ++ show x

patternBindings :: Pattern -> Value -> Maybe (M.Map T.Text Value)
patternBindings (VarP n) v = Just $ M.fromList [(n, v)]

patternBindings (IntP n) (IntV v)
	| v == n = Just M.empty
	| otherwise = Nothing
patternBindings (IntP n) _ = Nothing

patternBindings (BoolP b) (BoolV v)
	| v == b = Just M.empty
	| otherwise = Nothing

patternBindings (StrP x) (StrV y)
	| x == y = Just M.empty
	| otherwise = Nothing
patternBindings (StrP _) _ = Nothing

-- cons on strings
-- x:[] matches with y:""
patternBindings (ConsP x (ListP [])) (StrV str) =
	case T.uncons str of
		Just (y, ys) | T.null ys -> -- str matches y:[]
			patternBindings x (StrV $ T.singleton y)
		_ -> Nothing
-- "xy":xs pattern
patternBindings (ConsP (StrP xp) xsp) (StrV str) =
	let len = T.length xp in
	if T.take len str == xp then -- matches
		patternBindings xsp $ StrV (T.drop len str) -- match the rest of the string
	else Nothing -- no match
patternBindings (ConsP xp xsp) (StrV str) =
	case T.uncons str of
		Just (x, xs) -> do
			xe <- patternBindings xp (StrV $ T.singleton x)
			xse <- patternBindings xsp $ StrV xs
			Just $ M.union xe xse
		_ -> Nothing

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

patternBindings p x = error $ "patternBindings failure: matching " ++ show x ++ " with pattern " ++ show p

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
						env <- get
						let newenv = bindings:env
						put newenv
						eval body
				Nothing -> -- doesn't satisfy this pattern
					apply' xs

apply (Builtin (BIF fn)) arg = fn arg

-- some helper programs for evaluation

-- sets up stdin/stdout for binary mode and makes them unbuffered
initIO :: IO ()
initIO = do
       hSetBinaryMode stdin True
       hSetBinaryMode stdout True
       hSetBuffering stdout NoBuffering

-- Takes an interpreter state and evaluates it with the empty initial state.
interpret :: InterpState a -> IO a
interpret state = evalStateT state initialState

evalProgram :: [AST] -> InterpState Value
evalProgram nodes = foldl1' (>>) $ map eval nodes

evalString :: T.Text -> InterpState Value
evalString program =
	case parseProgram program of
		Left err -> error $ show err
		Right prg -> evalProgram prg

isLiterate :: FilePath -> Bool
isLiterate path = snd (splitExtension path) == ".lilamb"

-- Takes the lines of a literate program and returns the lines for a new executable program
-- from lines beginning with four spaces.
parseLiterate :: [T.Text] -> [T.Text]
parseLiterate lns = [T.drop 4 line | line <- lns, T.take 4 line == T.pack "    "]

evalFile :: FilePath -> InterpState Value
evalFile path = do
	contents <- liftIO $ if path == "-" then TIO.getContents else TIO.readFile path
	if isLiterate path then
		evalString . T.unlines . parseLiterate . T.lines $ contents
	else evalString contents

evalFileV :: FilePath -> IO Value
evalFileV = interpret . evalFile