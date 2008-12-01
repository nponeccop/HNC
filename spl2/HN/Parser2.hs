-----------------------------------------------------------------------------------------
{-| Module      : Main
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module HN.Parser2 (
	program, parseString, application, expression, mySepBy, 
	atom2, newExpression, simpleDefinition, whereClause, parseProg) where
import Text.Parsec.Prim
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Text.Parsec.Char

import HN.Intermediate
import Utils
import Control.Monad

pzero = parserZero

parseString p input
    = runP p () "test.hn0" (packL input)
 
identifier 
	= liftM2 (:) letter $ many $ letter <|> digit  

literal = between q q (many $ noneOf "\"") where q = (char '"') 
	
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
	do
		a <- many1 digit
		return $ Constant (ConstInt (read a))
	<|> 
	do
		a <- literal
		return $ Constant (ConstString a)
	<|>
	lambda
	<|>
	(try application)
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
		b <- mySepBy atom2 sep
		return b
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
	do
		(newExpression def)
		<|>
		(do
			b <- expression
			return $ def b [])
			
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
