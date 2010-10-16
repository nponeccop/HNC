module Test.Tests (tests) where
import HN.Intermediate
import HN.Optimizer.Utils
import Test.TestFixtures
import Test.HUnit

t a b c = (,,) a b c

compilerTests = "graphCompiler" ~: stt compilerTest
	[ t "L1 = 2" "L1 = 2\n" $ sv "a" 2
	, t "L1 = let L2 = 3 in 2" "L1 = 2\nL2 = 3\n" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 2
	, t "L1 = let L2 = 3 in L2" "L1 = L2\nL2 = 3\n" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ Atom "L2"
	, t "L1 = let b = 3 in let b = 4 in b" "L1 = L3\nL2 = 3\nL3 = 4\n" $ Definition "L1" [] $ Let (sv "b" 3) $ Let (sv "b" 4) $ In $ Atom "b"
	, t
		"L1 = let L2 = 3 in let L3 = 4 in L2"
		"L1 = L2\nL2 = 3\nL3 = 4\n" $
		Definition "L1" [] $ Let (sv "L2" 3) $ Let (sv "L3" 4) $ In $ Atom "L2"
	, t
		"L1 = let b = 3 in let L3 = (let b = 4 in b) in b"
	 	"L1 = L2\nL2 = 3\nL3 = L4\nL4 = 4\n" $
		Definition "L1" [] $ Let (sv "b" 3) $ Let (Definition "L3" [] $ Let (sv "b" 4) $ In $ Atom "b") $ In $ Atom "b"
	, t
		"L1 L2 = 2"
		"L1 L2 = 2\nL2 :: @\n" $
		Definition "L1" ["L2"] $ In $ ci 2
	, t
		"L1 L2 = L2 5"
		"L1 L2 = L2 5\nL2 :: @\n" $
		Definition "L1" ["L2"] $ In $ Application (Atom "L2") [ci 5]
	]

tests = "HN.Optimizer" ~: compilerTests : tt1 : tt2 : tt3 : tt4 : decompilerTests : []

tt1 = "test1" ~: stt test1
	[ t "aaa" "LM (UM (fromList [(1,2)]))" $ Definition "a" [] $ In $ ci 2
	, t "aab" "LM (UM (fromList [(1,2),(2,1)]))" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ Atom "L2"
	, t "L1 = let L2 = 3 in 4" "LM (UM (fromList [(1,2)]))" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 2
	]

tt2 = "test2" ~: stt test2
	[ t "L1 = 2" "L1 = 2\n" $ Definition "L1" [] $ In $ ci 2
	, t "L1 = let L2 = 2 in L2" "L1 = 2\nL2 = 2\n" $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, t "L1 = let L2 L3 = L3 in L2 4" "L1 = 4\nL2 L3 = L3\nL3 :: @\n" $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	, t "L1 = let L2 = 3 in 4" "L1 = 4\n" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 4
	]

tt3 = "test3" ~: stt test3
	[ t "L1 = 2" "L1 = 2\n" $ Definition "L1" [] $ In $ ci 2
	, t "L1 = let L2 = 2 in L2" "L1 = 2\n" $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, t "L1 = let L2 L3 = L3 in L2 4" "L1 = 4\n" $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	]

tt4 = "test4" ~: stt test4
	[ t "L1 = 2"  "LM (UM (fromList []))" $ Definition "L1" [] $ In $ ci 2
	, t "L1 = let L2 = 2 in L2" "LM (UM (fromList [(2,L1)]))" $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, t "L1 = let L2 L3 = L3 in L2 4" "LM (UM (fromList [(2,L1),(3,L2)]))" $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	, t "L1 = let L2 = 3 in 4" "LM (UM (fromList []))" $ Definition "L1" [] $ Let (sv "L2" 3) $ In $ ci 4
	]

xd = undefined

ee ((,,) x _ b) = (,,) x b b

decompilerTests = "graphDecompiler" ~: stt decompilerTest $ map ee
	[ t "L1 = 2" xd $ Definition "L1" [] $ In $ ci 2
	, t "L1 = let L2 = 2 in L2" xd $ Definition "L1" [] $ Let (Definition "L2" [] $ In $ ci 2) $ In $ Atom "L2"
	, t "L1 = let L2 L3 = L3 in L2 4" xd $ Definition "L1" [] $ Let (Definition "L2" ["L3"] $ In $ Atom "L3" ) $ In $ Application (Atom "L2") [ci 4]
	]
