module CPP.CompileTools (compileDefinition, typecheckDefinition) where

import qualified Data.Map as M

import HN.SplExport
import CPP.Intermediate
import SPL.Top
import SPL.Check3
import qualified Bar as AG

compileDefinition self  = AG.compile self inh where
	P (typedCodeTree, _, rootDefinitionType) = typecheckDefinition self
	inh = AG.Inh_Definition {
			AG.level_Inh_Definition = 0
		, 	AG.typed_Inh_Definition = typedCodeTree
		,   AG.symTab_Inh_Definition = M.map (const $ CppFqMethod "ff") SPL.Top.get_types
		,   AG.inferredType_Inh_Definition = rootDefinitionType
		}

typecheckDefinition self = check1 (convertDef self) SPL.Top.get_types
