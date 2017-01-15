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

argument = constant <|> parens <|> atom

atom = Atom <$> identifier

simpleExpression = constant <|> try application <|> atom

mySepBy atom2 sep = do
	a <- atom2
	bb <- try (sep >> mySepBy atom2 sep) <|> return []
	return $ a : bb

parens = between (char '(') (char ')') application

application =
	do
		a <- function
		char ' '
		b <- mySepBy argument $ char ' '
		return $ Application a b

function = atom	<|> parens

compoundExpression = between nlIndent2 nlDedent2 $ do
	x <- mySepBy definition nlIndent
	nlIndent
	xx <- simpleExpression
	return $ makeLet xx x

indent = many $ char '\t'

_assignment = do
	indent
	i <- identifier
	string " := "
	Assign i . In <$> simpleExpression

definition = do
	indent
	(fn:args) <- mySepBy identifier (char ' ')
	string " = "
	Definition fn args <$> (compoundExpression <|> (In <$> simpleExpression))

nlIndent = do
	nl
	indent
	return ""

nlIndent2 = void $ do
	char '{'
	string " " <|> (nl >> indent)

nlDedent2 = void $ do
	string " " <|> (nl >> indent)
	char '}'

program = sepBy definition nl

