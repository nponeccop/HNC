module Main (main) where
import HN.Optimizer.Intermediate
import HN.Optimizer.Utils
import HN.Optimizer.TestFixtures

compilerTests = stt compilerTest
	[ T "L1 = 2" "L1 = 2\n" $ sv "a" 2
	, T "L1 = let L2 = 3 in 2" "L1 = 2\nL2 = 3\n" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 2
	, T "L1 = let L2 = 3 in L2" "L1 = L2\nL2 = 3\n" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ Atom "L2"
	, T "L1 = let b = 3 in let b = 4 in b" "L1 = L3\nL2 = 3\nL3 = 4\n" $ Definition "L1" [] $ Let (sv "b" 3) $ Let (sv "b" 4) $ In $ Atom "b"
	, T
		"L1 = let L2 = 3 in let L3 = 4 in L2"
		"L1 = L2\nL2 = 3\nL3 = 4\n" $
		Definition "L1" [] $ Let (sv "L2" 3) $ Let (sv "L3" 4) $ In $ Atom "L2"
	, T
		"L1 = let b = 3 in let L3 = (let b = 4 in b) in b"
	 	"L1 = L2\nL2 = 3\nL3 = L4\nL4 = 4\n" $
		Definition "L1" [] $ Let (sv "b" 3) $ Let (Definition "L3" [] $ Let (sv "b" 4) $ In $ Atom "b") $ In $ Atom "b"
	, T
		"L1 L2 = 2"
		"L1 L2 = 2\nL2 :: @\n" $
		Definition "L1" ["L2"] $ In $ ci 2
	, T
		"L1 L2 = L2 5"
		"L1 L2 = L2 5\nL2 :: @\n" $
		Definition "L1" ["L2"] $ In $ Application (Atom "L2") [ci 5]
	]

main =	simpleTests (compilerTests ++ tests ++ tt2 ++ tt3 ++ tt4 ++ decompilerTests)

tests = stt test1
	[ T "aaa" "LM (UM (fromList [(1,2)]))" $ Definition "a" [] $ In $ ci 2
	, T "aab" "LM (UM (fromList [(1,2),(2,1)]))" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ Atom "L2"
	, T "L1 = let L2 = 3 in 4" "LM (UM (fromList [(1,2)]))" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 2
	]

tt2 = stt test2
	[ T "L1 = 2" "L1 = 2\n" $ Definition "L1" [] $ In $ ci 2
	, T "L1 = let L2 = 2 in L2" "L1 = 2\nL2 = 2\n" $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, T "L1 = let L2 L3 = L3 in L2 4" "L1 = 4\nL2 L3 = L3\nL3 :: @\n" $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	, T "L1 = let L2 = 3 in 4" "L1 = 4\n" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 4
	]

tt3 = stt test3
	[ T "L1 = 2" "L1 = 2\n" $ Definition "L1" [] $ In $ ci 2
	, T "L1 = let L2 = 2 in L2" "L1 = 2\n" $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, T "L1 = let L2 L3 = L3 in L2 4" "L1 = 4\n" $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	]

tt4 = stt test4
	[ T "L1 = 2"  "LM (UM (fromList []))" $ Definition "L1" [] $ In $ ci 2
	, T "L1 = let L2 = 2 in L2" "LM (UM (fromList [(2,L1)]))" $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, T "L1 = let L2 L3 = L3 in L2 4" "LM (UM (fromList [(2,L1),(3,L2)]))" $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	, T "L1 = let L2 = 3 in 4" "LM (UM (fromList []))" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 4
	]

xd = undefined

ee (T x _ b) = T x b b

decompilerTests = stt decompilerTest $ map ee
	[ T "L1 = 2" xd $ Definition "L1" [] $ In $ ci 2
	, T "L1 = let L2 = 2 in L2" xd $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, T "L1 = let L2 L3 = L3 in L2 4" xd $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	]
