-- AST definition for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

module AST where
import qualified Data.Text as T

data AST = Add AST AST
		 | Sub AST AST
		 | Mul AST AST
		 | Div AST AST
		 | Equals AST AST
		 | NotEquals AST AST
		 | LessThan AST AST
		 | GreaterThan AST AST
		 | Block [AST]
		 | FunDef T.Text (Pattern, AST)
		 | Defun T.Text AST
		 | Def Pattern AST
		 | Var T.Text
		 | Lambda [(Pattern, AST)]
		 | Call AST AST
		 | Access AST AST
		 | Cons AST AST
		 | IfExpr AST AST AST
		 | TupleConst [AST]
		 | ListConst [AST]
		 | BoolConst Bool
		 | StrConst T.Text
		 | IntConst Integer
		 deriving (Show, Eq)

data Pattern = VarP T.Text
			 | IntP Integer
			 | StrP T.Text
			 | BoolP Bool
			 | ConsP Pattern Pattern
			 | TupleP [Pattern]
			 | ListP [Pattern]
	 deriving (Show, Eq)