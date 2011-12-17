module CPP.CompileTools (compile2, compile, compile3) where

import qualified Bar as AG
import HN.Parser2
import FFI.TypeParser
import Utils

compile inFile f = fmap (f . fromRight) $ parseFile inFile

compile2 f  = compile3 "lib/lib.hni" f

compile3 hniFileName f inFile = do
	libraryTypes <- importHni hniFileName
	compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . joinStr "\n" . map show . compileDefinition2 libraryTypes . f

compileDefinition2 libraryTypes self = AG.compile2 self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = libraryTypes
	}

