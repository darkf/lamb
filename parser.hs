-- Parser for the Lamb programming language
-- Copyright (c) 2013 darkf
-- Licensed under the terms of the zlib license, see LICENSE for details

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
						  T.reservedNames = ["do", "end"],
						  T.reservedOpNames = ["+", "-", "*", "/"]}

lexer = T.makeTokenParser languageDef
exprparser = buildExpressionParser ops term <?> "expression"
ops = [ [Infix (reservedOp "*" >> return Mul) AssocLeft]
	  , [Infix (reservedOp "/" >> return Div) AssocLeft]
	  , [Infix (reservedOp "+" >> return Add) AssocLeft]
	  , [Infix (reservedOp "-" >> return Sub) AssocLeft]
	  ]

identifier = T.identifier lexer
reserved   = T.reserved   lexer
reservedOp = T.reservedOp lexer
parens     = T.parens     lexer
integer    = T.integer    lexer
semi       = T.semi       lexer
whiteSpace = T.whiteSpace lexer
symbol	   = T.symbol lexer

statement = exprparser

-- http://codereview.stackexchange.com/a/2572
stringChar =
	escaped <|> noneOf "\""
	where
		escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
		escapedChar code replacement = char code >> return replacement
		codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
		replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

stringLiteral = do
	char '"'
	x <- many stringChar
	char '"'
	whiteSpace
	return x

block = do
	reserved "do"
	lst <- seqStmt
	reserved "end"
	return $ Block lst

listSeq p cons = do
	symbol "["
	lst <- sepBy p (symbol ",")
	symbol "]"
	return $ cons lst

intPattern = fmap IntP integer
varPattern = fmap VarP identifier
listPattern = listSeq pattern ListP

consPattern = do
	x <- intPattern <|> varPattern
	symbol "::"
	y <- pattern
	return $ ConsP x y

pattern = try consPattern
		<|> listPattern
	    <|> varPattern
	    <|> intPattern

patterns = sepBy pattern (symbol ",")

funDef = do
	name <- identifier
	symbol "("
	pats <- patterns
	let pats' = if pats == [] then [UnitP] else pats -- at least Unit
	symbol ")"
	symbol "->"
	lst <- exprparser
	return $ rewriteFun (FunDef name (pats', lst))

-- curry FunDef to a definition of lambdas
rewriteFun (FunDef name (patterns, body)) =
	Defun name lam
	where
		-- curry it
		lam = foldr (\pat lam -> Lambda [(pat, [lam])]) body patterns

call = do
	name <- identifier
	whiteSpace
	symbol "("
	args <- sepBy exprparser (symbol ",")
	let args' = if args == [] then [UnitConst] else args -- at least Unit
	symbol ")"
	return $ Call name args'

consExpr = do
	x <- expr'
	symbol "::"
	y <- exprparser
	return $ Cons x y

expr' = try block
	 <|> try funDef
	 <|> try call
	 <|> parens exprparser
	 <|> listSeq exprparser ListConst
	 <|> fmap Var identifier
	 <|> fmap StrConst stringLiteral
	 <|> fmap IntConst integer

term = try consExpr
	<|> expr'

seqStmt = sepBy1 statement semi

program =
	many1 $ do
		e <- exprparser
		symbol "."
		return e

parseProgram = parse program "program"