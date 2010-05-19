module CPP.CompileTools (compileDefinition, typecheckDefinition) where

import qualified Data.Map as M

import HN.SplExport
import CPP.Intermediate
import SPL.Top
import qualified Bar as AG

compileDefinition self  = AG.compile self inh where
	inh = AG.Inh_Definition {
			AG.level_Inh_Definition = 0
		, 	AG.typed_Inh_Definition = undefined
		,   AG.symTab_Inh_Definition = M.map (const $ CppFqMethod "ff") SPL.Top.get_types
		, 	AG.inhCounter_Inh_Definition = 0
		, 	AG.visibleAtoms_Inh_Definition = AG.instantiateLibrary SPL.Top.get_types
		}

typecheckDefinition self = check1 (convertDef self) SPL.Top.get_types
