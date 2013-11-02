{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy
import AST

[peggy|
top :: AST = expr !.

expr :: AST
  = expr "+" fact { Add $1 $2 }
  / expr "-" fact { Sub $1 $2 }
  / fact

fact :: AST
  = fact "*" term { Mul $1 $2 }
  / fact "/" term { Div $1 $2 }
  / term

term :: AST
  = "(" expr ")"
  / number

number ::: AST
  = [1-9] [0-9]* { IntConst $ read ($1 : $2) }
|]

main :: IO ()
main = print . parseString top "<stdin>" =<< getContents