module Main where

import SPL.Types
import HN.Intermediate
import CPP.CompileTools


comp (CNum n) =
	Constant $ ConstInt n
comp (CVal v) =
	Atom v
comp (CL c (K p)) =
	Application (comp c) (Prelude.map comp p)
comp (CL c (S p)) =
	Lambda p (comp c)
--comp (CL c (W ((n,v):ws))) =
--	Application (comp c) (Prelude.map comp p)
comp o =
	-- Constant $ ConstInt 0
	error $ show o

c = CL (CL (CL (CVal "foldr") (K [CVal "g",CVal "elist",CVal "l"])) (W [("g",CL (CL (CVal "join1") (K [CL (CVal "f") (K [CVal "x"]),CVal "y"])) (S ["x","y"]))])) (S ["f","l"])

main = print $ compileDefinition $ Definition "main" [] (comp c) []
res2 = Definition "main" [] (comp c) []
