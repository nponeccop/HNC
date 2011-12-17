{-# LANGUAGE NoMonomorphismRestriction #-}

module FFI.TypeParser (sp3, decl, fun, parseType, typePolyVar, typeVar, importHni) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import qualified Data.Map as M
import Control.Monad ((>=>))

import Utils
import qualified HN.Parser2 as P
import SPL.Types
import HN.Parser2

importHni = readFile >=> return . M.fromList . map parseDecl . lines

parseDecl = sp3 decl

sp3 x = fromRight . P.parseString x

typePolyVar = fmap TU $ string "?" >> identifier

typeVar = fmap TV $ string "??" >> identifier

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

simpleType = fmap T identifier

parseType = try fun <|> try paramType <|> simpleType <|> try typeVar <|> typePolyVar
