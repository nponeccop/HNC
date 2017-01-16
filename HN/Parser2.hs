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

constant = Constant <$> (
	(ConstInt . read <$> many1 digit)
	<|>
	(ConstString <$> literal))

atom = Atom <$> identifier

simpleExpression x = do
	xx <-
		constant
		<|>
		(parens >>= app)
		<|>
		do
			a <- atom
			app a <|> return a
	return $ makeLet xx x

app a = Application a <$> (many1 . try  $ char ' ' >> (function <|> constant))

parens = between (char '(') (char ')') (function >>= app)

function = atom	<|> parens

compoundExpression = between nlIndent2 nlDedent2 $ do
	x <- many1 . try $ do
		a <- definition
		nlIndent
		return a
	simpleExpression x

indent = many $ char '\t'

_assignment = do
	indent
	i <- identifier
	string " := "
	Assign i <$> simpleExpression []

definition = do
	indent
	fn <- try identifier
	args <- many . try $ char ' ' >> identifier
	string " = "
	Definition fn args <$> (compoundExpression <|> simpleExpression [])

nlIndent = nl >> indent

nlIndent2 = void $ do
	char '{'
	string " " <|> nlIndent

nlDedent2 = void $ do
	string " " <|> nlIndent
	char '}'

program = sepBy definition nl

