{-# LANGUAGE NoMonomorphismRestriction #-}

module HN.TypeParser where

import Text.Parsec.Prim
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Text.Parsec.Char
import qualified Data.Map as M
import Intermediate
import Utils
import qualified HN.Parser2 as P
import HN.MyTypeCheck

pair a b = (a, b)

pf x y =  y >>= (return . x) 

simpleParse2 prog = fromRight $ P.parseString parseType prog

typeVar = do
	a <- many1 digit
	return $ TypeVar $ read a
	
fun = pf Fun $ sepBy parseSimpleType (string " -> ")
	
parens = do
	char '('
	a <- parseType
	char ')'
	return a

simpleType = pf Type $ many1 letter
	
adt = pf g (sepBy parseSimpleType $ char ' ') where
	g a = case a of 
		[b] -> b
		_ -> ADT a

parseSimpleType = typeVar <|> simpleType <|> parens

parseType = try fun <|> adt
