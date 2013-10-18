import Control.Monad.State (State, runState, evalState, get, put)
import qualified Data.Map as M

data AST = Add AST AST
		 | Def String AST
		 | Var String
		 | StrConst String
		 | IntConst Int
		 deriving (Show, Eq)

data Value = IntV Int
		   | StrV String
		   deriving (Show, Eq)

type Env = M.Map String Value -- an environment
type InterpState = State Env -- interpreter state (pass along the global environment)

(IntV l) +$ (IntV r) = IntV (l + r)
(StrV l) +$ (StrV r) = StrV (l ++ r)
l +$ r = error $ "cannot + " ++ show l ++ " and " ++ show r

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

evalProgram :: [AST] -> Value -- fold the state from each node and return the result
evalProgram nodes = evalState (foldr1 (>>) $ map eval nodes) M.empty

main = do
	print $ evalProgram prg
	print $ evalProgram prg2
	where
		prg = [Def "x" (IntConst 5),
			   Def "y" (IntConst 3),
			   Add (Var "x") (Var "y")]
		prg2 = [Add (StrConst "hi ") (StrConst "there")]