module CPP.CompileTools (compileDefinition, typecheckDefinition) where

import qualified Data.Map as M

import HN.SplExport
import CPP.Intermediate
import SPL.Top
import SPL.Check3
import qualified Bar as AG

compileDefinition self  = tdi2 typed x self where
	P (typed, _, x) = typecheckDefinition self

	tdi2 typed inferredType self = AG.compile self AG.Inh_Definition {
			AG.level_Inh_Definition = 0
		, 	AG.typed_Inh_Definition = typed
		,   AG.symTab_Inh_Definition = M.map (const $ CppFqMethod "ff") SPL.Top.get_types
		,   AG.inferredType_Inh_Definition = inferredType
		}

typecheckDefinition self = check1 (convertDef self) SPL.Top.get_types
