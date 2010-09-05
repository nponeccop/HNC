module CPP.CompileTools (compileDefinition, compileDefinition2, typecheckDefinition) where

import HN.SplExport
import SPL.Top
import qualified Bar as AG

compileDefinition self = compileDefinition2 SPL.Top.get_types self

compileDefinition2 libraryTypes self = AG.compile2 self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = libraryTypes
	}

typecheckDefinition self = check1 (convertDef self) SPL.Top.get_types
