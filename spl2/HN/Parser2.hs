-----------------------------------------------------------------------------------------
{-| Module      : Main
    Copyright   :
    License     : All Rights Reserved

    Maintainer  :
    Stability   :
    Portability :
-}
-----------------------------------------------------------------------------------------

module HN.Parser2 (
	program, parseString, application, expression, mySepBy,
	atom2, newExpression, simpleDefinition, whereClause, parseProg, parseFile) where
import Text.Parsec.Prim
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Text.Parsec.Char

import HN.Intermediate
import Utils
import Control.Monad

pzero = parserZero

parseString p = runP p () "test.hn0" . packL

xletter = letter <|> char '_'

identifier
	= liftM2 (:) xletter $ many $ xletter <|> digit

literal = between q q $ many $ noneOf "\"" where q = char '"'

constructLambda h (Lambda args ex) = Lambda (h:args) ex

lambdaTail =
	do
		h <- identifier
		char ' '
		t <- lambdaTail
		return $ constructLambda h t
	<|>
	do
		string "-> "
		e <- expression
		return $ Lambda [] e

lambda = do
	string "\\ "
	lambdaTail

expression =
	(many1 digit >>= return . Constant . ConstInt . read)
	<|>
	(literal >>= return . Constant . ConstString)
	<|>
	lambda
	<|>
	try application
	<|>
	atom2

argument =
	do
		a <- many1 digit
		return $ Constant (ConstInt $ read a)
	<|>
	do
		a <- literal
		return $ Constant (ConstString a)
	<|>
	parens
	<|>
		do
			string "where"
			parserZero
	<|>
	atom

atom = do
	a <- identifier
	return $ Atom a

atom2 =
		do
			try (string "where")
			pzero
		<|>
		atom

mySepBy atom2 sep = try (do
	a <- atom2
	bb <- do
		sep
		mySepBy atom2 sep

	return (a : bb))
	<|>
	do
		a <- atom2
		return [a]

parens = do
	char '('
	x <- (lambda <|> application)
	char ')'
	return x


application =
	do
		a <- function
		string " "
		b <- mySepBy argument $ char ' '
		return $ Application a b

function = atom	<|> parens

newExpression def = do
	char '{'
	string " " <|> nlIndent
	x <- mySepBy simpleDefinition nlIndent
	nlIndent
	xx <- expression
	string " " <|> nlIndent
	char '}'
	return $ def xx x

simpleDefinition = 	do
	many $ char '\t'
	parms <- mySepBy identifier (char ' ')
	string " = "
	let def = Definition (head parms) (tail parms)
	(
		newExpression def
		<|>
		(do
			b <- expression
			return $ def b []))

nlIndent = do
	many1 $ string "\n"
	many $ string "\t"
	return []



whereClause = do
	string " where\n"
	p <- program
	string ";"
	return p

simple =
	do
		a <- simpleDefinition
		w <- option [] whereClause
		return $ case a of Definition a p b x -> Definition a p b (x ++ w)


program = sepBy simple (many1 $ string "\n")

parseProg = parseString program

parseFile = parseFromFile program