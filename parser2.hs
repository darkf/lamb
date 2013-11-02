{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy hiding (space)
import AST

[peggy|
top :: [AST] = statements !.

lineComment :: () = '--' (!'\n' .)* '\n' { () }

space :: () = [ \r\n\t] { () } / lineComment

statements :: [AST] = statement+

statement :: AST = expr "."

semistatements :: [AST]
  = expr ";" semistatements { $1 : $2 }
  / expr { [$1] }

args :: AST
  = expr ("," expr)+ { TupleConst ($1 : $2) }
  / expr? { case $1 of
  				Just x -> x
  				Nothing -> UnitConst }

patternlist :: Pattern
  = pattern ("," pattern)+ { ListP ($1 : $2) }
  / pattern? { case $1 of
  				Just x -> ListP [x]
  				Nothing -> ListP [] }

pattern :: Pattern
  = pattern "::" pattern { ConsP $1 $2 }
  / "[" patternlist "]"
  / integer { IntP $1 }
  / stringlit { StrP $1 }

funpattern :: Pattern
  = pattern ("," pattern)+ { TupleP ($1 : $2) }
  / pattern? { case $1 of
  					Just x -> x
  					Nothing -> UnitP }

listseq :: AST
  = expr ("," expr)+ { ListConst ($1 : $2) }
  / expr? { case $1 of
  				Just x -> ListConst [x]
  				Nothing -> ListConst [] }

doblock :: AST
  = "do" semistatements "end" { Block $1 }

expr :: AST
  = expr "(" args ")" { Call $1 $2 }
  / expr "::" expr { Cons $1 $2 }
  / expr "+" fact { Add $1 $2 }
  / expr "-" fact { Sub $1 $2 }
  / "[" listseq "]"
  / identifier "(" funpattern ")" "->" expr { Defun $1 (Lambda [($2, $3)]) }
  / fact

fact :: AST
  = fact "*" term { Mul $1 $2 }
  / fact "/" term { Div $1 $2 }
  / term

term :: AST
  = "(" expr ")"
  / doblock
  / stringlit { StrConst $1 }
  / integer { IntConst $1 }
  / identifier { Var $1 }

stringlit ::: String = '\"' charlit* '\"'

charlit :: Char
  = '\\' escChar
  / [^\"\\]

escChar :: Char
  = '\"' { '\"' }
  / '\\' { '\\' }
  / '/' { '/' }
  / 'b' { '\b' }
  / 'f' { '\f' }
  / 'n' { '\n' }
  / 'r' { '\r' }
  / 't' { '\t' }

identifier ::: String
  = [a-zA-Z_] [a-zA-Z0-9_'?!]* { $1 : $2 }

integer ::: Integer
  = [1-9] [0-9]* { read ($1 : $2) }
|]

main :: IO ()
main = print . parseString top "<stdin>" =<< getContents