import Control.Monad.State (State, runState, evalState, get, put)
import qualified Data.Map as M

data AST = Add AST AST
		 | Def String AST
		 | Var String
		 | Call String [AST]
		 | StrConst String
		 | IntConst Int
		 deriving (Show, Eq)

data Pattern = VarP String
	 deriving (Show, Eq)

data Value = IntV Int
		   | StrV String
		   | FnV [ ([Pattern], [AST]) ] -- pattern->body bindings
		   deriving (Show, Eq)

type Env = M.Map String Value -- an environment
type InterpState = State Env -- interpreter state (pass along the global environment)

(IntV l) +$ (IntV r) = IntV (l + r)
(StrV l) +$ (StrV r) = StrV (l ++ r)
l +$ r = error $ "cannot + " ++ show l ++ " and " ++ show r

initialState = M.fromList [("id", FnV [([VarP "x"], [Var "x"])])]

eval :: AST -> InterpState Value

eval (IntConst i) = return $ IntV i
eval (StrConst s) = return $ StrV s

eval (Var var) = get >>= \m ->
  case M.lookup var m of
    Just v -> return v
    Nothing -> error $ "unbound variable " ++ var

eval (Def name v') = do
	v <- eval v'
	m <- get
	put $ M.insert name v m
	return v

eval (Add l r) = do
	l <- eval l
	r <- eval r
	return $ l +$ r

eval (Call name args) = get >>= \m ->
	case M.lookup name m of
	  Just fn@(FnV _) ->
	  	do
	  		xargs <- mapM eval args
	  		apply fn xargs
	  Nothing -> error $ "call: name " ++ name ++ " doesn't exist or is not a function"

patternBindings :: Pattern -> Value -> Maybe Env
patternBindings (VarP n) v = Just $ M.fromList [(n, v)]

-- applies a function
apply :: Value -> [Value] -> InterpState Value
apply (FnV pats) args =
	apply' pats args
	where
		apply' (([pat], body):xs) (arg:argxs) = -- todo: more than one argument
			case patternBindings pat arg of
				Just env' -> -- satisfies
					do
						env <- get
						put $ M.union env env'
						foldr1 (>>) $ map eval body
				Nothing -> error "doesn't satisfy"

evalProgram :: [AST] -> Value -- fold the state from each node and return the result
evalProgram nodes = evalState (foldr1 (>>) $ map eval nodes) initialState

main = do
	print $ evalProgram prg
	print $ evalProgram prg2
	print $ evalProgram prg3
	where
		prg = [Def "x" (IntConst 5),
			   Def "y" (IntConst 3),
			   Add (Var "x") (Var "y")]
		prg2 = [Add (StrConst "hi ") (StrConst "there")]
		prg3 = [Call "id" [IntConst 20]]