import Control.Monad.State (State, runState, evalState, get, put)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import AST
import Parser (parseProgram)

data Value = IntV Integer
		   | StrV String
		   | UnitV
		   | StreamV Int
		   | ListV [Value]
		   | FnV [(Pattern, [AST])] -- pattern->body bindings
		   deriving (Show, Eq)

type Env = M.Map String Value -- an environment
type InterpState = State ([BS.ByteString], Env) -- interpreter state

(IntV l) +$ (IntV r) = IntV (l + r)
(StrV l) +$ (StrV r) = StrV (l ++ r)
l +$ r = error $ "cannot + " ++ show l ++ " and " ++ show r

initialState = ([], M.fromList [("id", FnV [(VarP "x", [Var "x"])]),
						   		("stdout", StreamV 0)])

eval :: AST -> InterpState Value

eval (IntConst i) = return $ IntV i
eval (StrConst s) = return $ StrV s

eval UnitConst = return UnitV

eval (ListConst v) =
	mapM eval v >>= \xs ->
		return $ ListV xs

eval (Var var) = get >>= \(_,m) ->
  case M.lookup var m of
    Just v -> return v
    Nothing -> error $ "unbound variable " ++ var

eval (Def name v') = do
	v <- eval v'
	(s,m) <- get
	put (s, M.insert name v m)
	return v

eval (Lambda pats) =
	return $ FnV pats

eval (Add l r) = do
	l <- eval l
	r <- eval r
	return $ l +$ r

eval (Call name args) = get >>= \(_,m) ->
	case M.lookup name m of
	  Just fn@(FnV _) ->
	  	do
	  		xargs <- mapM eval args
	  		applyMany fn xargs
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

-- applies many arguments to a function
applyMany :: Value -> [Value] -> InterpState Value
applyMany fn@(FnV _) (arg:xs) =
	apply fn arg >>= \value ->
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
						put (s, M.union env env')
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

main = do
	print $ evalProgram prg
	print $ evalProgram prg2
	print $ evalProgram prg3
	print $ evalProgram prg4
	print $ evalString "f() -> 5+2. f()."
	print $ evalString "f([x, y, z]) -> z. f([1, 2, 3])."
	where
		prg = [Def "x" (IntConst 5),
			   Def "y" (IntConst 3),
			   Add (Var "x") (Var "y")]
		prg2 = [Add (StrConst "hi ") (StrConst "there")]
		lam arg body = Lambda [(VarP arg, [body])]
		prg3 = [ Def "add" (lam "x" $ lam "y" $ Add (Var "x") (Var "y")),
				 Def "f" $ Lambda [
				 					(IntP 0, [IntConst 100]),
				 					(IntP 1, [IntConst 200]),
				 					(VarP "x", [IntConst 300])
				 				  ],
				 Call "f" [IntConst 2]]
		prg4 = [ Def "lst" (ListConst [IntConst 1, IntConst 2, IntConst 3]),
				 Def "f" $ Lambda [
				 				(ListP [VarP "x", VarP "y", VarP "z", VarP "w"], [Var "w"]),
				 				(ConsP (VarP "x") (ConsP (VarP "y") (VarP "ys")), [Var "ys"])
				 		   ],
				 Call "f" [Var "lst"]]