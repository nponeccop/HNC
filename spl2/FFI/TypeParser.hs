{-# LANGUAGE NoMonomorphismRestriction #-}

module FFI.TypeParser (parseDecl, sp3, decl, fun, parseType, typePolyVar, typeVar) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Utils
import qualified HN.Parser2 as P
import SPL.Types
import HN.Parser2


parseDecl = sp3 decl

pf x y =  y >>= (return . x)

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

paramType = do
	a <- identifier
	string "<"
	b <- sepBy parseType $ string " "
	string ">"
	return $ TD a b

parseType2 = parens <|> try paramType <|> simpleType <|> try typeVar <|> typePolyVar

parens = do
	char '('
	a <- parseType
	char ')'
	return a

simpleType = pf T $ identifier

parseType = try fun <|> try paramType <|> simpleType <|> try typeVar <|> typePolyVar
