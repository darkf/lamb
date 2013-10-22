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

tupleSeq p cons = do
	symbol "("
	lst <- sepBy1 p (symbol ",")
	symbol ")"
	return $ cons lst

emptyTuple cons = do
	symbol "("
	symbol ")"
	return $ cons []

intPattern = fmap IntP integer
varPattern = fmap VarP identifier
listPattern = listSeq pattern ListP

consPattern = do
	x <- intPattern <|> varPattern
	symbol "::"
	y <- pattern
	return $ ConsP x y

pattern = try consPattern
		<|> try (emptyTuple TupleP)
		<|> try (tupleSeq pattern TupleP)
		<|> listPattern
	    <|> varPattern
	    <|> intPattern

patterns = sepBy pattern (symbol ",")

funDef = do
	name <- identifier
	symbol "("
	pats <- patterns
	let pat = (case pats of
		[] -> UnitP
		[a] -> a
		otherwise -> TupleP pats)
	symbol ")"
	symbol "->"
	body <- exprparser
	return $ Defun name $ Lambda [(pat, body)]

call = do
	name <- identifier
	whiteSpace
	symbol "("
	args <- sepBy exprparser (symbol ",")
	let arg = (case args of
		[] -> UnitConst
		[a] -> a
		otherwise -> TupleConst args)
	symbol ")"
	return $ Call name arg

consExpr = do
	x <- expr'
	symbol "::"
	y <- exprparser
	return $ Cons x y

expr' = try block
	 <|> try funDef
	 <|> try call
	 <|> try (emptyTuple TupleConst)
	 <|> try (tupleSeq exprparser TupleConst)
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