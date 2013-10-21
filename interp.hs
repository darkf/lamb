-- Interpreter for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

module Interp where
import Prelude hiding (lookup)
import qualified Data.Map as M
import Control.Monad.State (State, runState, evalState, get, put)
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
		   | StreamV Int
		   | ListV [Value]
		   | Builtin BIF
		   | FnV [(Pattern, [AST])] -- pattern->body bindings
		   deriving (Show, Eq)

type Env = M.Map String Value -- an environment
type InterpState = State ([Handle], Env) -- interpreter state (open handles, global env)

lookup :: Env -> String -> Maybe Value
lookup env name = M.lookup name env

bind :: Env -> String -> Value -> Env
bind env name value = M.insert name value env

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

-- these are pretty nasty and instead of using unsafePerformIO
-- we could throw eval, etc. into StateT with IO instead, but then
-- everything would be in IO.

_putstr (StrV str) = do
	(handles,_) <- get
	let stdout_s = head handles
	let io = unsafe_putstr stdout_s str
	return $ seq io UnitV
	where
		{-# NOINLINE unsafe_putstr #-}
		unsafe_putstr h s = unsafePerformIO $ hPutStr h s >> hFlush h

_getline UnitV = do
	(handles,_) <- get
	let stdin_s = handles !! 1
	let str = unsafe_getline stdin_s
	return $ seq () $ StrV str
	where
		{-# NOINLINE unsafe_getline #-}
		unsafe_getline h = unsafePerformIO $ hGetLine h

_itos (IntV i) = return $ StrV $ show i
_itos v = error $ "itos: not an int: " ++ show v

initialState = ([stdout, stdin],
					M.fromList [("id", FnV [(VarP "x", [Var "x"])]),
						   		("stdout", StreamV 0),
						   		("putstr", Builtin $ BIF _putstr),
						   		("putstrln", Builtin $ BIF (\x -> _putstr $ x +$ StrV "\n")),
						   		("itos", Builtin $ BIF _itos),
						   		("getline", Builtin $ BIF _getline)])

eval :: AST -> InterpState Value

eval (IntConst i) = return $ IntV i
eval (StrConst s) = return $ StrV s

eval UnitConst = return UnitV

eval (Block body) = foldr1 (>>) $ map eval body

eval (ListConst v) =
	mapM eval v >>= \xs ->
		return $ ListV xs

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
			let FnV oldpats = oldfn
			    Lambda [(pat, body)] = fn
			    newfn = FnV ((pat, body):oldpats) in
			    put (s, bind env name newfn) >> return newfn

eval (Def name v') = do
	v <- eval v'
	(s,env) <- get
	put (s, bind env name v)
	return v

eval (Lambda pats) =
	return $ FnV pats

eval (Add l r) = do { l <- eval l; r <- eval r; return $ l +$ r }
eval (Sub l r) = do { l <- eval l; r <- eval r; return $ l -$ r }
eval (Mul l r) = do { l <- eval l; r <- eval r; return $ l *$ r }
eval (Div l r) = do { l <- eval l; r <- eval r; return $ l /$ r }

eval (Call name args) = get >>= \(_,env) ->
	case lookup env name of
	  Just fn@(FnV _) ->
	  	do
	  		xargs <- mapM eval args
	  		applyMany fn xargs
	  Just fn@(Builtin _) -> mapM eval args >>= applyMany fn
	  Nothing -> error $ "call: name " ++ name ++ " doesn't exist or is not a function"

patternBindings :: Pattern -> Value -> Maybe Env
patternBindings (VarP n) v = Just $ M.fromList [(n, v)]

patternBindings (IntP n) (IntV v)
	| v == n = Just M.empty
	| otherwise = Nothing
patternBindings (IntP n) _ = Nothing

patternBindings UnitP UnitV = Just M.empty
patternBindings UnitP _ = Nothing

patternBindings (ConsP x (ListP [])) (ListV (y:[])) = patternBindings x y
patternBindings (ConsP _ _) (ListV (_:[])) = Nothing
patternBindings (ConsP xp xsp) (ListV (x:xs)) =
	do
		xe <- patternBindings xp x
		xse <- patternBindings xsp $ ListV xs
		Just $ M.union xe xse
patternBindings (ConsP _ _) _ = Nothing

patternBindings (ListP []) (ListV (x:xs)) = Nothing -- not enough patterns
patternBindings (ListP (_:_)) (ListV []) = Nothing -- not enough values
patternBindings (ListP []) (ListV []) = Just M.empty -- base case
patternBindings (ListP (x:xs)) (ListV (y:ys)) =
	do
		env <- patternBindings x y
		env' <- patternBindings (ListP xs) (ListV ys)
		Just $ M.union env' env
patternBindings (ListP _) _ = Nothing -- not a list

-- applies many arguments to a function
applyMany :: Value -> [Value] -> InterpState Value
applyMany fn@(FnV _) (arg:xs) =
	apply fn arg >>= \value ->
		applyMany value xs
applyMany (Builtin (BIF fn)) (arg:xs) =
	fn arg >>= \value ->
		applyMany value xs
applyMany value [] = return value
applyMany _ xs = error "couldn't apply all arguments"

-- applies a function
apply :: Value -> Value -> InterpState Value
apply (FnV pats) arg =
	apply' pats
	where
		apply' [] = error $ "argument " ++ show arg ++ " doesn't satisfy any patterns"
		apply' ((pat, body):xs) =
			case patternBindings pat arg of
				Just env' -> -- satisfies
					do
						(s,env) <- get
						put (s, M.union env' env)
						foldr1 (>>) $ map eval body
				Nothing -> -- doesn't satisfy this pattern
					apply' xs

evalProgram :: [AST] -> Value -- fold the state from each node and return the result
evalProgram nodes = evalState (foldr1 (>>) $ map eval nodes) initialState

evalString :: String -> Value
evalString program =
	case parseProgram program of
		Left err -> error $ show err
		Right prg -> evalProgram prg
