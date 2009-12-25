module HN.ParserTest (runTests, runTest2) where

import HN.Parser2
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import HN.Intermediate

xx = mySepBy simpleDefinition (string "\n")

simpleEq a b w = Definition a [] (Atom b) w

newExpressionT d = newExpression $ Definition d []

numbers = many hexDigit

runTest program _in expectedOut = putStr $ case parseString program _in of
		(Left error)  -> "\nParseError\n\n" ++ _in ++ "\n\n" ++ show error ++ "\n\n\n"
		(Right parsed) -> runTest2 parsed _in expectedOut

runTest2 actualOut _in expectedOut = if actualOut == expectedOut then "." else failedTest _in expectedOut actualOut

failedTest _in expectedOut parsed = "\nWrong result\n\n" ++ _in ++ "\n- " ++ show expectedOut ++ "\n- " ++ show parsed ++ "\n"

rtp = runTest program

semicolon = string " ; "

ppp = many semicolon

pppp = sepBy numbers semicolon

rtp2 p1 p2 x = do
	rtp p1 x
	rtp p2 x

testNewExpression = do
	runTest (newExpressionT "aaa") "{ a = b\nc }" (simpleEq "aaa" "c" [simpleEq "a" "b" []])
	runTest simpleDefinition "aaa = { a = b\nc }" (simpleEq "aaa" "c" [Definition "a" [] (Atom "b") []])
	rtp "aaa = { a = b\nc }" [(Definition "aaa" [] (Atom "c") [Definition "a" [] (Atom "b") []])]
	rtp "a = { c = d\ne = f\nb }" [Definition "a" [] (Atom "b") [Definition "c" [] (Atom "d") [], Definition "e" [] (Atom "f") []]]
	runTest (newExpressionT "a") "{ c = d\ne = f\nb }" (Definition "a" [] (Atom "b") [Definition "c" [] (Atom "d") [], Definition "e" [] (Atom "f") []])
	runTest xx "c = d\ne = f\nb }" [simpleEq "c" "d" [], simpleEq "e" "f" []]
	rtp "a b c = d" [Definition "a" ["b", "c"] (Atom "d") []]

runTests = do
	testNewExpression

	runTest semicolon " ; " " ; "
	runTest ppp " ; " [" ; "]
	runTest pppp "1 ; 2" ["1", "2"]

	rtp "c = e" [simpleEq "c" "e" []]
	rtp "a = b\nc = d" [simpleEq "a" "b" [],simpleEq "c" "d" []]
	runTest whereClause " where\nc = d;" [simpleEq "c" "d" []]

	rtp2 "a = { c = d\nb }\ne = f" "a = b where\nc = d;\ne = f" [simpleEq "a" "b" [simpleEq "c" "d" []],simpleEq "e" "f" []]
	rtp2 "a = { c = d\ne = f\nb }\ng = h" "a = b where\nc = d\ne = f;\ng = h" [simpleEq "a" "b" [simpleEq "c" "d" [],simpleEq "e" "f" []],simpleEq "g" "h" []]
	rtp2 "a = { c = { e = f\nd }\nb }" "a = b where\nc = d where\ne = f;;" [simpleEq "a" "b" [simpleEq "c" "d" [simpleEq "e" "f" []]]]
	rtp2 "a = { c = d\ne = f\nb }\ng = { i = jj\nh }" "a = b where\nc = d\ne = f;\ng = h where\ni = jj;"   [simpleEq "a" "b" [simpleEq "c" "d" [],simpleEq "e" "f" []],simpleEq "g" "h" [simpleEq "i" "jj" []]]

	runTest expression "\\ b -> c" $ Lambda ["b"] (Atom "c")

	runTest expression "\\ b c -> c" $ Lambda ["b","c"] (Atom "c")
	rtp "a b c = d" [Definition "a" ["b", "c"] (Atom "d") []]

	rtp2 "a = { c = \\ d -> ef\ne = f\nb }\ng = { i = jj\nh }" "a = b where\nc = \\ d -> ef\ne = f;\ng = h where\ni = jj;" [simpleEq "a" "b" [Definition "c" [] (Lambda ["d"] (Atom "ef")) [],simpleEq "e" "f" []],simpleEq "g" "h" [simpleEq "i" "jj" []]]

	rtp "a = b c" [Definition "a" [] (Application (Atom "b") [Atom "c"]) []]
	rtp "a = b (c d)" [Definition "a" [] (Application (Atom "b") [Application (Atom "c") [Atom "d"]]) []]

	rtp "a b = c" [Definition "a" ["b"] (Atom "c") []]
	rtp "a = \\ b -> c" [Definition "a" [] (Lambda ["b"] (Atom "c")) []]

	rtp "a = b (\\ c -> d)" [Definition "a" [] (Application (Atom "b") [Lambda ["c"] (Atom "d")]) []]
	rtp "a = { f c = d\nb f }" [Definition "a" [] (Application (Atom "b") [Atom "f"]) [Definition "f" ["c"] (Atom "d") []]]

	rtp "a = b ((c f) d)" [Definition "a" [] (Application (Atom "b") [Application (Application (Atom "c") [Atom "f"]) [Atom "d"]]) []]
	runTest application "(b c) d" $ Application (Application (Atom "b") [Atom "c"]) [Atom "d"]
	runTest expression "(b c) d" $ Application (Application (Atom "b") [Atom "c"]) [Atom "d"]
	rtp "a = (b c) d" [Definition "a" [] (Application (Application (Atom "b") [Atom "c"]) [Atom "d"]) []]
	runTest application "b c d" $ Application (Atom "b") [Atom "c",Atom "d"]

	rtp "a = b c d" [Definition "a" [] (Application (Atom "b") [Atom "c",Atom "d"]) []]

	-- тесты Вага
	rtp "f = \"a\"" [Definition "f" [] (Constant (ConstString "a")) []]

	rtp "f = \\ z -> 1" [Definition "f" [] (Lambda ["z"] (Constant (ConstInt 1))) []]
	rtp "f z = 1" [Definition "f" ["z"] (Constant (ConstInt 1)) []]

	runTest application "u 6" $ Application (Atom "u") [Constant (ConstInt 6)]
	rtp "f = u 6" [Definition "f" [] (Application (Atom "u") [Constant (ConstInt 6)]) []]
	-- тесты из Tests.hs
	rtp "plusx = \\ i -> plus x i" [Definition "plusx" [] (Lambda ["i"] (Application (Atom "plus") [Atom "x",Atom "i"])) []]
	rtp "plusx i = plus x i" [Definition "plusx" ["i"] (Application (Atom "plus") [Atom "x",Atom "i"]) []]

	runTest application "mul (plusx y) (plusx z)" $ Application (Atom "mul") [Application (Atom "plusx") [Atom "y"],Application (Atom "plusx") [Atom "z"]]
	runTest expression "\\ x y z -> mul (plusx y) (plusx z)" $ Lambda ["x","y","z"] (Application (Atom "mul") [Application (Atom "plusx") [Atom "y"],Application (Atom "plusx") [Atom "z"]])

	rtp2 "main = { y = g\nf }" "main = f where\ny = g;" [simpleEq "main" "f" [simpleEq "y" "g" []]]

	-- тесты mySepBy и связанного бага с распознаванием where
	runTest (mySepBy atom2 (string " ")) "a b" [Atom "a", Atom "b"]
	runTest (mySepBy atom2 (string " ")) "a b !" [Atom "a", Atom "b"]
	runTest (mySepBy atom2 (string " ")) "a b where" [Atom "a", Atom "b"]

	runTest application "f y where" $ Application (Atom "f") [Atom "y"]
	runTest expression "f y where" $ Application (Atom "f") [Atom "y"]

	rtp2 "main = { y = g x\nf y }" "main = f y where\ny = g x;" [Definition "main" [] (Application (Atom "f") [Atom "y"]) [Definition "y" [] (Application (Atom "g") [Atom "x"]) []]]

	rtp2 "main = { y = \\ z -> g z\nf y y }" "main = f y y where\ny = \\ z -> g z;" [Definition "main" [] (Application (Atom "f") [Atom "y",Atom "y"]) [Definition "y" [] (Lambda ["z"] (Application (Atom "g") [Atom "z"])) []]]
	rtp "main = { y z = g z\nf y y }" [Definition "main" [] (Application (Atom "f") [Atom "y",Atom "y"]) [Definition "y" ["z"] (Application (Atom "g") [Atom "z"]) []]]

	rtp2 "main = { y = \\ z -> g z\n\\ x -> f y y }" "main = \\ x -> f y y where\ny = \\ z -> g z;" [Definition "main" [] (Lambda ["x"] (Application (Atom "f") [Atom "y",Atom "y"])) [Definition "y" [] (Lambda ["z"] (Application (Atom "g") [Atom "z"])) []]]
	rtp "main f = { y z = g z\nf y y }" [Definition "main" ["f"] (Application (Atom "f") [Atom "y",Atom "y"]) [Definition "y" ["z"] (Application (Atom "g") [Atom "z"]) []]]

	rtp2 "main = { y = g x\n\\ x -> f y y }" "main = \\ x -> f y y where\ny = g x;" [Definition "main" [] (Lambda ["x"] (Application (Atom "f") [Atom "y",Atom "y"])) [Definition "y" [] (Application (Atom "g") [Atom "x"]) []]]
	rtp "main x = { y = g x\nf y y }" [Definition "main" ["x"] (Application (Atom "f") [Atom "y",Atom "y"]) [Definition "y" [] (Application (Atom "g") [Atom "x"]) []]]

	-- to flush line-buffered stdout
	putStrLn "";
