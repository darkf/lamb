-- AST definition for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

module AST where

data AST = Add AST AST
		 | Sub AST AST
		 | Mul AST AST
		 | Div AST AST
		 | Block [AST]
		 | FunDef String (Pattern, AST)
		 | Defun String AST
		 | Def String AST
		 | Var String
		 | Lambda [(Pattern, AST)]
		 | Call String AST
		 | UnitConst
		 | Cons AST AST
		 | IfExpr AST AST AST
		 | TupleConst [AST]
		 | ListConst [AST]
		 | StrConst String
		 | IntConst Integer
		 deriving (Show, Eq)

data Pattern = VarP String
			 | IntP Integer
			 | UnitP
			 | ConsP Pattern Pattern
			 | TupleP [Pattern]
			 | ListP [Pattern]
	 deriving (Show, Eq)