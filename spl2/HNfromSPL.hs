module HNfromSPL where

import SPL.Types
import SPL.Top
import SPL.Check3
import HN.Intermediate
import HN.SplExport
import CPP.Core
import CPP.Intermediate

import Data.Map as M

baseToTdi = M.map (const $ CppFqMethod "ff") SPL.Top.get_types

tdi rt = DefinitionInherited {
        diLevel        = 3
,       diSymTab       = baseToTdi
,       diType         = Nothing
,       diTraceP       = False
,       diRootTypes    = rt
}

compile =
	(dsCppDef . z)
		where
		z self @ (Definition name _ _ _) = sem_Definition (tdi types) self
			where
			P (fv, x) = check1 (convertDef self) SPL.Top.get_types
			types = M.insert name x fv


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

res = compile $ Definition "main" [] (comp c) []
res2 = Definition "main" [] (comp c) []


