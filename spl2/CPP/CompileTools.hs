module CPP.CompileTools (compileDefinition, typecheckDefinition) where

import HN.SplExport
import SPL.Top
import qualified Bar as AG

compileDefinition self = AG.compile2 self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = SPL.Top.get_types
	}

typecheckDefinition self = check1 (convertDef self) SPL.Top.get_types
