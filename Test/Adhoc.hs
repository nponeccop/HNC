
module Main where

-- import Visualise
import Utils

import HN.Parser2
import HN.Intermediate
import HN.SplExport

import CPP.Visualise ()

import qualified SPL.Top
import Test.HnParser (testSet)

simpleParse  = head . fromRight . parseProg

-- testCodeGen = rt compileDefinition

-- test2 = rt getDefinitionFreeVars

rt f = mapM (print . f . simpleParse) testSet

	{-
defaultEnv = Env 1 $ M.fromList $ map (\(a, b) -> (a, simpleParse2 b)) [
		("head",  "(List 1) -> 1" )
	,	("plus1", "Int -> Int" )
	]
-}
testCheck3 = mapM (print . SPL.Top.check0 . convertExpr) [
		Constant (ConstInt 123),
		Atom "a",
		Application (Atom "sum") $ map (Constant . ConstInt) [1, 2],
		Application (Atom "incr") [Atom "x"]
	]

main = do
	putStrLn  "\n***\nNOTE all the 3 commended tests compile but crash in check3!!!!!!!!\n***\n\n"
	testCheck3
	rt convertDef
	rt $ SPL.Top.check0 . convertDef
	return ()
