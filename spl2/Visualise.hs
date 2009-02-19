module Main where

import Test.HUnit

import SPL.Visualise
import SPL.Parser
import SPL.Compiler


fullParse t = case (parse t) of P _ _ x -> remove_cdebug $ compile x

ttt x = TestCase $ assertEqual (x ++ "\n" ++ show fp) x $ showAsSource fp where
	fp = fullParse x

tests = TestList $ map ttt [
		"sum"
	,	"sum a"
	,	"sum a b"
	,	"sum a 2"
	,	"z*z"
	,	"x y*y:5"
	,	"x y*y:5*x:incr"
	,	"x y*y:5*x:(z*incr z)"
	,	"z*incr z"
	,	"a*b*sum a b"
	,	"sum a (incr b)"
	,	"a*b*sum a (incr b)"
	,	"foo x*foo:2"
	,	"(foo bar*foo:incr)*bar:incr"
	]

main = runTestTT tests
