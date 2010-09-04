{-# LANGUAGE NoMonomorphismRestriction #-}

module HN.TypeParser where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Utils
import qualified HN.Parser2 as P
import SPL.Types
import HN.Parser2

pair a b = (a, b)

pf x y =  y >>= (return . x)

simpleParse2 = fromRight . P.parseString parseType

sp3 x = fromRight . P.parseString x

typePolyVar = string "?" >> identifier >>=	return . TU

typeVar = string "??" >> identifier >>= return . TV

decl = do
	a <- identifier
	string " = "
	b <- parseType
	return (a, b)

fun = do
	a <- many1 $ do
 		x <- parseType2
		string " "
		return x
	string "-> "
	b <- parseType2
	return $ TT $ a ++ [b]

parseType2 = simpleType

parens = do
	char '('
	a <- parseType
	char ')'
	return a

simpleType = pf T $ identifier

parseSimpleType = typeVar <|> simpleType <|> parens

parseType = try fun <|> simpleType <|> try typeVar <|> typePolyVar
