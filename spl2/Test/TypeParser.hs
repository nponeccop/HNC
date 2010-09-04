module Test.TypeParser where

import Test.HUnit

import HN.TypeParser
import Utils
import SPL.Types

t a b c d = st a b $ sp3 c d

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
	]