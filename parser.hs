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

statement = exprparser

block = do
	reserved "do"
	lst <- seqStmt
	reserved "end"
	return $ Block lst

intPattern = fmap IntP integer
varPattern = fmap VarP identifier

consPattern = do
	x <- intPattern <|> varPattern
	symbol "::"
	y <- pattern
	return $ ConsP x y

pattern = option UnitP $
			try consPattern
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
	return $ rewriteFun (FunDef name (pats, lst))

-- curry FunDef to a definition of lambdas
rewriteFun (FunDef name (patterns, body)) =
	Def name lam
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

term = try block
	 <|> try funDef
	 <|> try call
	 <|> parens exprparser
	 <|> fmap Var identifier
	 <|> fmap IntConst integer

seqStmt = sepBy1 statement semi

program =
	many1 $ do
		e <- exprparser
		symbol "."
		return e

parseProgram = parse program "program"