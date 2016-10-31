module Test.TypeParser (tests, iotests) where

import Test.HUnit

import FFI.TypeParser
import Utils
import SPL.Types
import FFI.Visualise
import SPL.Top
import Test.ParserTest (parserTest)

t a b c d = st a b $ sp3 c d

t1 name = t ("full-" ++ name ++ ": " ++ showType tt) (name, tt) decl $ name ++ " = " ++ showType tt where
	tt = tracedUncondLookup "aaa" name get_types

tests = [
	    t "typePolyVar" (TU "aaa") typePolyVar "?aaa"
	,   t "typeVar" (TV "bbb") typeVar "??bbb"
	,   t "parseType-TU" (TU "aaa") parseType "?aaa"
	,   t "parseType-TV" (TV "bbb") parseType "??bbb"
	,   t "simpleDecl1" ("baba211", TU "iddqd") decl "baba211 = ?iddqd"
	,   t "simpleDecl2" ("b", T "idDqd") decl "b = idDqd"
	,   t "fun" (TT [T "a", T "b"]) fun "a -> b"
	,   t "manyArgs" (TT [T "a", T "b", T "b"]) fun "a b -> b"
	,   t "manyArgs-decl" ("foo", TT [T "a", T "b", T "b"]) decl "foo = a b -> b"
	,   t1 "eq"
	,	t1 "_if"
	,	t1 "natrec"
	,   t1 "bind"
	]

tt =
	let
		a ?= b = a ~=? parserTest parseType b
	in
	[ T "aaa" ?= "aaa"
	, T "aaa" ?= "aaa bbb" -- TODO failing test
	, TT [T "aaa", T "bbb"] ?= "aaa -> bbb"
	, TT [T "List", T "a", T "a"] ?= "List a -> a" -- TODO dubious test
	, TT [T "Int", T "Int"] ?= "Int -> Int"
	]

iotests = do
	a <- readFile "lib/lib.hni"
	return $ map process (lines a) ++ tt 

process l = st "lib.hni" l (name ++ " = " ++ showType tt) where
	(name, tt) = sp3 decl l
