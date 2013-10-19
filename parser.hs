module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import AST

languageDef = emptyDef {T.commentStart="{-",
						  T.commentEnd="-}",
						  T.commentLine="--",
						  T.nestedComments=True,
						  T.identStart = letter <|> char '_',
						  T.identLetter = alphaNum <|> char '_',
						  T.reservedNames = ["do", "end", "(", ")", ";", "."],
						  T.reservedOpNames = ["+", "*"]}

lexer = T.makeTokenParser languageDef
exprparser = buildExpressionParser ops term <?> "expression"
ops = [ [Infix (reservedOp "*" >> return Mul) AssocLeft ]
	  , [Infix (reservedOp "+" >> return Add) AssocLeft]
	  ]

identifier = T.identifier lexer -- parses an identifier
reserved   = T.reserved   lexer -- parses a reserved name
reservedOp = T.reservedOp lexer -- parses an operator
parens     = T.parens     lexer -- parses surrounding parenthesis
integer    = T.integer    lexer -- parses an integer
semi       = T.semi       lexer -- parses a semicolon
whiteSpace = T.whiteSpace lexer -- parses whitespace
symbol	   = T.symbol lexer

statement =
  do
	e <- exprparser
	-- char ';'
	return e 

block = do
	reserved "do"
	lst <- seqStmt
	reserved "end"
	return $ Block lst

pattern = option UnitP $
	        fmap VarP identifier
	    <|> fmap IntP integer

funDef = do
	name <- identifier
	symbol "("
	pat <- pattern
	symbol ")"
	symbol "->"
	lst <- exprparser
	return $ rewriteFun (FunDef name (pat, lst))

-- curry FunDef to a definition of lambdas
rewriteFun (FunDef name (pattern, body)) =
	Def name $ Lambda [(pattern, [body])]

call = do
	name <- identifier
	whiteSpace
	symbol "("
	symbol ")"
	return $ Call name [UnitConst]

term = try block
	 <|> try funDef
	 <|> try call
	 <|> parens exprparser
	 <|> fmap Var identifier
	 <|> fmap IntConst integer

manyExpr = many1 exprparser

seqStmt = do
	lst <- sepBy1 statement semi
	return lst

program =
	many1 $ do
		e <- exprparser
		reserved "."
		return e

parseProgram = parse program "program"