module AST where

data AST = Add AST AST
		 | Sub AST AST
		 | Mul AST AST
		 | Div AST AST
		 | Block [AST]
		 | FunDef String ([Pattern], AST)
		 | Defun String AST
		 | Def String AST
		 | Var String
		 | Lambda [(Pattern, [AST])]
		 | Call String [AST]
		 | UnitConst
		 | ListConst [AST]
		 | StrConst String
		 | IntConst Integer
		 deriving (Show, Eq)

data Pattern = VarP String
			 | IntP Integer
			 | UnitP
			 | ConsP Pattern Pattern
			 | ListP [Pattern]
	 deriving (Show, Eq)