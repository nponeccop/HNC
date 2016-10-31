module Main (main) where
import Test.QuickCheck
import Test.HUnit

import SPL.Parser2
import SPL.Compiler
import SPL.Top

import SPL.Visualise

import Utils
import Test.Compiler

import Test.SPL
import Test.FFI
import Test.TypeParser

import Test.Tests
import Test.ParserTest
import Test.Optimizer.FileTest

import SPL.Interpretator
import Test.SPLQuickCheck
import qualified Test.HnParser

ttt x = TestCase $ assertEqual (x ++ "\n" ++ show fp) True $ typeCheck fp where
	fp = fullParse x

fullParse t = remove_cdebug $
	case (case parse t of SPL.Parser2.P _ _ x _ -> compile0 x ; SPL.Parser2.N _ _ -> error "QuickCheck.fullParse.1: Parsing failed") of SPL.Compiler.P xx -> xx ; SPL.Compiler.N _ aa -> error $ show aa

tests = map ttt [
		"(less (incr 2*foo:2) 2*foo:less (incr 2*foo:2) 2)*foo:1b"
	,	"((incr 2*foo:2)*foo:(incr 2*foo:2))*foo:1b"
	]


ttt2 x = TestCase $ assertEqual (x ++ "\n" ++ show fp) x $ showAsSource fp where
	fp = fullParse x

tests2 = map ttt2 [
		"sum"
	,	"a*sum a"
	,	"a*b*sum a b"
	,	"a*sum a 2"
	,	"z*z"
	,	"x*x y*y:5"
	,	"x y*y:5*x:incr"
	,	"x y*y:5*x:(z*incr z)"
	,	"z*incr z"
	,	"a*b*sum a b"
	,	"a*b*sum a (incr b)"
	,	"foo*x*foo x*foo:2"
	,	"foo*bar*(foo bar*foo:incr)*bar:incr"
	]


splTest (s, r, t) = TestLabel s $ TestCase $ case step s of
	SPL.Interpretator.P (t2, r2) -> do
		assertEqual "Types mismatch" t t2
		assertEqual "Result mismatch" r r2
	SPL.Interpretator.N (_, r3) ->
		assertEqual "Result mismatch (error)" r r3


splTests = map splTest Test.SPL.tests

main = do
	optTests <- Test.Optimizer.FileTest.iotests
	compilerTests <- Test.Compiler.iotests
	ioTests <- Test.TypeParser.iotests
	simpleTests $
		optTests ++
		Test.HnParser.tests ++
		tests2 ++
		Main.tests ++
		compilerTests ++
		splTests
		++ Test.FFI.tests
		++ Test.TypeParser.tests
		++ ioTests
		++ [Test.Tests.tests] 
		++ [Test.ParserTest.tests]
	  	++ []
	putStrLn "QuickCheck :"
	Test.QuickCheck.quickCheckWith ( stdArgs { maxSuccess = 50}) prop_Foo
