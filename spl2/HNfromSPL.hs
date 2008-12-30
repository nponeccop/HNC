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

compile = (dsCppDef . z) where
	z self @ (Definition name _ _ _) = sem_Definition (tdi types) self where
			P (fv, x) = check1 (convertDef self) SPL.Top.get_types
			types = M.insert name x fv


comp (CNum n) =
	Definition "num" [] (Constant $ ConstInt n) []

res = compile $ comp $ (CNum 1)


