{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy
import AST

[peggy|
top :: [AST] = statements !.

statements :: [AST]
  = statement+

statement :: AST
  = expr "."

expr :: AST
  = expr "(" ")" { Call $1 UnitConst }
  / expr "+" fact { Add $1 $2 }
  / expr "-" fact { Sub $1 $2 }
  / fact

fact :: AST
  = fact "*" term { Mul $1 $2 }
  / fact "/" term { Div $1 $2 }
  / term

term :: AST
  = "(" expr ")"
  / number { IntConst $1 }
  / identifier { Var $1 }

identifier ::: String
  = [a-zA-Z_] [a-zA-Z0-9_'?!]* { $1 : $2 }

number ::: Integer
  = [1-9] [0-9]* { read ($1 : $2) }
|]

main :: IO ()
main = print . parseString top "<stdin>" =<< getContents