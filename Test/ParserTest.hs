{-# LANGUAGE FlexibleContexts #-}
module Test.ParserTest (runTest2, tests, parserTest, parseString) where
-- module Test.ParserTest (runTests, runTest2, tests, parserTest) where

-- import HN.Parser2
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Utils (packL)
-- import HN.Intermediate

import Test.HUnit

---------------------------------------------------
-- New test suite
---------------------------------------------------

parseString p = runP p () "test.hn0" . packL

parserTest s _in = case parseString s _in of
	Right x -> x
	Left x -> error $ show x

rtt name parser _in out = name ~: out ~=? parserTest parser _in

-- rttp name = rtt name program


---------------------------------------------------
-- Old adhoc tests
---------------------------------------------------

{-
xx = mySepBy simpleDefinition (string "\n")

simpleEq a b w = Definition a [] $ makeLet (Atom b) w

newExpressionT d = newExpression $ \a b -> Definition d [] $ makeLet a b
-}
numbers = many hexDigit

{-
runTest program _in expectedOut = putStr $ case parseString program _in of
		(Left error)  -> "\nParseError\n\n" ++ _in ++ "\n\n" ++ show error ++ "\n\n\n"
		(Right parsed) -> runTest2 parsed _in expectedOut
-}
runTest2 actualOut _in expectedOut = if actualOut == expectedOut then "." else failedTest _in expectedOut actualOut

failedTest _in expectedOut parsed = "\nWrong result\n\n" ++ _in ++ "\n- " ++ show expectedOut ++ "\n- " ++ show parsed ++ "\n"

--rtp = runTest program

semicolon = string " ; "

ppp = many semicolon

pppp = sepBy numbers semicolon
{-
rtp2 p1 p2 x = do
	rtp p1 x
	rtp p2 x

testNewExpression = do
	runTest (newExpressionT "aaa") "{ a = b\nc }" (simpleEq "aaa" "c" [simpleEq "a" "b" []])
	runTest simpleDefinition "aaa = { a = b\nc }" (simpleEq "aaa" "c" [Definition "a" [] $ makeLet (Atom "b") []])
	rtp "aaa = { a = b\nc }" [Definition "aaa" [] $ makeLet (Atom "c") [Definition "a" []  $ makeLet  (Atom "b") []]]
	rtp "a = { c = d\ne = f\nb }" [Definition "a" []  $ makeLet  (Atom "b") [Definition "c" []  $ makeLet (Atom "d") [], Definition "e" []  $ makeLet (Atom "f") []]]
	runTest (newExpressionT "a") "{ c = d\ne = f\nb }" (Definition "a" [] $ makeLet  (Atom "b") [Definition "c" [] $ makeLet (Atom "d") [], Definition "e" []  $ makeLet (Atom "f") []])
	runTest xx "c = d\ne = f\nb }" [simpleEq "c" "d" [], simpleEq "e" "f" []]
	rtp "a b c = d" [Definition "a" ["b", "c"]  $ makeLet (Atom "d") []]
-}

tests = "Parser" ~:
	[ rtt "semicolon" semicolon " ; " " ; "
	, rtt "ppp" ppp " ; " [" ; "]
	, rtt "pppp" pppp "1 ; 2" ["1", "2"]
{-	, rttp "eq" "c = e" [simpleEq "c" "e" []]
	, rttp "eq-eq" "a = b\nc = d" [simpleEq "a" "b" [],simpleEq "c" "d" []]
	, rtt "where" whereClause " where\nc = d;" [simpleEq "c" "d" []]
	, rtt "" expression "\\ b -> c" $ Lambda ["b"] (Atom "c")
	, rtt "" expression "\\ b c -> c" $ Lambda ["b","c"] (Atom "c")
	, rtt "" application "(b c) d" $ Application (Application (Atom "b") [Atom "c"]) [Atom "d"]
	, rtt "" expression "(b c) d" $ Application (Application (Atom "b") [Atom "c"]) [Atom "d"]
	, rtt "" application "b c d" $ Application (Atom "b") [Atom "c",Atom "d"]
	, rtt "" application "u 6" $ Application (Atom "u") [Constant (ConstInt 6)]
	, rtt "" application "mul (plusx y) (plusx z)" $ Application (Atom "mul") [Application (Atom "plusx") [Atom "y"],Application (Atom "plusx") [Atom "z"]]
	, rtt "" expression "\\ x y z -> mul (plusx y) (plusx z)" $ Lambda ["x","y","z"] (Application (Atom "mul") [Application (Atom "plusx") [Atom "y"],Application (Atom "plusx") [Atom "z"]])
	, rtt "" (mySepBy atom2 (string " ")) "a b" [Atom "a", Atom "b"]
	, rtt "" (mySepBy atom2 (string " ")) "a b !" [Atom "a", Atom "b"]
	, rtt "" (mySepBy atom2 (string " ")) "a b where" [Atom "a", Atom "b"]
	, rtt "" application "f y where" $ Application (Atom "f") [Atom "y"]
	, rtt "" expression "f y where" $ Application (Atom "f") [Atom "y"]
	, rttp "fundef" "a b c = d" [Definition "a" ["b", "c"] $ makeLet (Atom "d") []]
	, rttp "vardef" "a = b c" [Definition "a" [] $ makeLet (Application (Atom "b") [Atom "c"]) []]
	, rttp "paren" "a = b (c d)" [Definition "a" [] $ makeLet (Application (Atom "b") [Application (Atom "c") [Atom "d"]]) []]
	, rttp "atomBody" "a b = c" [Definition "a" ["b"] $ makeLet (Atom "c") []]
	, rttp "" "a = \\ b -> c" [Definition "a" [] $ makeLet (Lambda ["b"] (Atom "c")) []]
	, rttp "" "a = b (\\ c -> d)" [Definition "a" [] $ makeLet (Application (Atom "b") [Lambda ["c"] (Atom "d")]) []]
	, rttp "" "a = { f c = d\nb f }" [Definition "a" [] $ makeLet (Application (Atom "b") [Atom "f"]) [Definition "f" ["c"] $ makeLet (Atom "d") []]]
	, rttp "" "a = b ((c f) d)" [Definition "a" [] $ makeLet (Application (Atom "b") [Application (Application (Atom "c") [Atom "f"]) [Atom "d"]]) []]
	, rttp "" "a = (b c) d" [Definition "a" [] $ makeLet (Application (Application (Atom "b") [Atom "c"]) [Atom "d"]) []]
	, rttp "" "a = b c d" [Definition "a" [] $ makeLet (Application (Atom "b") [Atom "c",Atom "d"]) []]
	, rttp "" "f = \"a\"" [Definition "f" [] $ makeLet (Constant (ConstString "a")) []]
	, rttp "" "f = \\ z -> 1" [Definition "f" [] $ makeLet (Lambda ["z"] (Constant (ConstInt 1))) []]
	, rttp "" "f z = 1" [Definition "f" ["z"] $ makeLet (Constant (ConstInt 1)) []]
	, rttp "" "f = u 6" [Definition "f" [] $ makeLet (Application (Atom "u") [Constant (ConstInt 6)]) []]
	, rttp "" "plusx = \\ i -> plus x i" [Definition "plusx" [] $ makeLet (Lambda ["i"] (Application (Atom "plus") [Atom "x",Atom "i"])) []]
	, rttp "" "plusx i = plus x i" [Definition "plusx" ["i"] $ makeLet (Application (Atom "plus") [Atom "x",Atom "i"]) []]
	, rttp "" "main = { y z = g z\nf y y }" [Definition "main" [] $ makeLet (Application (Atom "f") [Atom "y",Atom "y"]) [Definition "y" ["z"] $ makeLet (Application (Atom "g") [Atom "z"]) []]]
	, rttp "" "main f = { y z = g z\nf y y }" [Definition "main" ["f"]  $ makeLet (Application (Atom "f") [Atom "y",Atom "y"]) [Definition "y" ["z"] $ makeLet (Application (Atom "g") [Atom "z"]) []]]
	, rttp "" "main x = { y = g x\nf y y }" [Definition "main" ["x"] $ makeLet (Application (Atom "f") [Atom "y",Atom "y"]) [Definition  "y" [] $ makeLet (Application (Atom "g") [Atom "x"]) []]]
-}
	]
{-
runTests = do
	testNewExpression

	rtp2 "a = { c = d\nb }\ne = f" "a = b where\nc = d;\ne = f" [simpleEq "a" "b" [simpleEq "c" "d" []],simpleEq "e" "f" []]
	rtp2 "a = { c = d\ne = f\nb }\ng = h" "a = b where\nc = d\ne = f;\ng = h" [simpleEq "a" "b" [simpleEq "c" "d" [],simpleEq "e" "f" []],simpleEq "g" "h" []]
	rtp2 "a = { c = { e = f\nd }\nb }" "a = b where\nc = d where\ne = f;;" [simpleEq "a" "b" [simpleEq "c" "d" [simpleEq "e" "f" []]]]
	rtp2 "a = { c = d\ne = f\nb }\ng = { i = jj\nh }" "a = b where\nc = d\ne = f;\ng = h where\ni = jj;"   [simpleEq "a" "b" [simpleEq "c" "d" [],simpleEq "e" "f" []],simpleEq "g" "h" [simpleEq "i" "jj" []]]


	rtp2 "a = { c = \\ d -> ef\ne = f\nb }\ng = { i = jj\nh }" "a = b where\nc = \\ d -> ef\ne = f;\ng = h where\ni = jj;" [simpleEq "a" "b" [Definition "c" [] $ makeLet (Lambda ["d"] (Atom "ef")) [],simpleEq "e" "f" []],simpleEq "g" "h" [simpleEq "i" "jj" []]]

	rtp2 "main = { y = g\nf }" "main = f where\ny = g;" [simpleEq "main" "f" [simpleEq "y" "g" []]]

	-- тесты mySepBy и связанного бага с распознаванием where

	rtp2 "main = { y = g x\nf y }" "main = f y where\ny = g x;" [Definition "main" [] $ makeLet (Application (Atom "f") [Atom "y"]) [Definition "y" [] $ makeLet (Application (Atom "g") [Atom "x"]) []]]

	rtp2 "main = { y = \\ z -> g z\nf y y }" "main = f y y where\ny = \\ z -> g z;" [Definition "main" [] $ makeLet (Application (Atom "f") [Atom "y",Atom "y"]) [Definition "y" [] $ makeLet (Lambda ["z"] (Application (Atom "g") [Atom "z"])) []]]

	rtp2 "main = { y = \\ z -> g z\n\\ x -> f y y }" "main = \\ x -> f y y where\ny = \\ z -> g z;" [Definition "main" [] $ makeLet (Lambda ["x"] (Application (Atom "f") [Atom "y",Atom "y"])) [Definition "y" [] $ makeLet (Lambda ["z"] (Application (Atom "g") [Atom "z"])) []]]

	rtp2 "main = { y = g x\n\\ x -> f y y }" "main = \\ x -> f y y where\ny = g x;" [Definition "main" []  $ makeLet (Lambda ["x"] (Application (Atom "f") [Atom "y",Atom "y"])) [Definition "y" [] $ makeLet (Application (Atom "g") [Atom "x"]) []]]
	-- to flush line-buffered stdout
	putStrLn "";
-}
