{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Parser where
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

patterntuple :: Pattern
  = "(" "," ")" { TupleP [] }
  / "(" pattern ("," pattern)+ ")" { TupleP ($1 : $2) }
  / "(" pattern "," ")" { TupleP [$1] }

pattern :: Pattern
  = pattern "::" pattern { ConsP $1 $2 }
  / "[" patternlist "]"
  / patterntuple
  / "true" { BoolP True } / "false" { BoolP False }
  / identifier { VarP $1 }
  / stringlit { StrP $1 }
  / integer { IntP $1 }

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

tuple :: AST
  = "(" "," ")" { TupleConst [] }
  / "(" expr ("," expr)+ ")" { TupleConst ($1 : $2) }
  / "(" expr "," ")" { TupleConst [$1] }

doblock :: AST
  = "do" semistatements "end" { Block $1 }

lambda :: AST
  = "\\" funpattern "->" expr { Lambda [($1, $2)] }

def :: AST
  = pattern "=" expr { Def $1 $2 }

ifcond :: AST
  = "if" expr "then" expr "else" expr { IfExpr $1 $2 $3 }

expr :: AST
  = expr "::" expr { Cons $1 $2 }
  / expr "+" fact { Add $1 $2 }
  / expr "-" fact { Sub $1 $2 }
  / expr "==" fact { Equals $1 $2 }
  / expr "!=" fact { NotEquals $1 $2 }
  / expr "<" fact { LessThan $1 $2 }
  / expr ">" fact { GreaterThan $1 $2 }
  / def
  / lambda
  / identifier "(" funpattern ")" "->" expr { Defun $1 (Lambda [($2, $3)]) }
  / fact

fact :: AST
  = fact "*" term { Mul $1 $2 }
  / fact "/" term { Div $1 $2 }
  / term

term :: AST
  = term "(" args ")" { Call $1 $2 }
  / tuple
  / "(" expr ")"
  / "[" listseq "]"
  / ifcond
  / doblock
  / "true" { BoolConst True } / "false" { BoolConst False }
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
  = [0-9] [0-9]* { read ($1 : $2) }
|]

parseProgram program = parseString top "<input>" program
