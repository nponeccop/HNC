-----------------------------------------------------------------------------------------
{-| Module      : Main
    Copyright   :
    License     : All Rights Reserved

    Maintainer  :
    Stability   :
    Portability :
-}
-----------------------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module HN.Parser2 (program, identifier) where
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char

import HN.Intermediate
import Control.Monad

identifier
	= liftM2 (:) xletter $ many $ xletter <|> digit where
		xletter = letter <|> char '_'

literal = between q q $ many $ noneOf "\"" where q = char '"' --"

nl = optional cr >> lf where
	cr = char '\r'
	lf = char '\n'

expression =
	(Constant . ConstInt . read) <$> many1 digit
	<|>
	(Constant . ConstString) <$> literal
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
			mzero
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
	x <- application
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

indent = many $ char '\t'

_assignment = do
	indent
	i <- identifier
	string " := "
	(Assign i . In) <$> expression


simpleDefinition = 	do
	indent
	parms <- mySepBy identifier (char ' ')
	string " = "
	let def v w = Definition (head parms) (tail parms) $ makeLet v w
	(
		newExpression def
		<|>
		(do
			b <- expression
			return $ def  b []))

nlIndent = do
	many1 nl
	many $ string "\t"
	return []



whereClause = do
	string " where"
	nl
	p <- program
	string ";"
	return p

simple =
	do
		a <- simpleDefinition
		w <- option [] whereClause
		return $ case a of Definition a p l -> Definition a p $ makeLet (letValue l) (w ++ letWhere l)


program = sepBy simple $ many1 nl

