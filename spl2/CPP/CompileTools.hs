module CPP.CompileTools (compileDefinition, typecheckDefinition) where

import qualified Data.Map as M

import HN.SplExport
import CPP.Intermediate
import SPL.Top
import qualified Bar as AG

compileDefinition self  = AG.compile self inh where
	inh = AG.Inh_Definition {
			AG.level_Inh_Definition = 0
		,   AG.inferredQualifiers_Inh_Definition = M.map (const $ CppFqMethod "ff") SPL.Top.get_types
		, 	AG.neighborQualifiers_Inh_Definition = M.map (const $ CppFqMethod "ff") SPL.Top.get_types
		, 	AG.inhCounter_Inh_Definition = 0
		, 	AG.visibleAtoms_Inh_Definition = SPL.Top.get_types
		, 	AG.inferredTypes_Inh_Definition = SPL.Top.get_types
		}

typecheckDefinition self = check1 (convertDef self) SPL.Top.get_types
