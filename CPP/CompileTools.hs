module CPP.CompileTools (parseHN, compileHN, compileFile) where

import qualified Bar as AG
import HN.Parser2 (parseFile) 
import Utils
import Control.Applicative

compileFile inFile libraryTypes = compileHN libraryTypes <$> parseHN inFile

parseHN inFile  = fromRight <$> parseFile inFile

compileHN libraryTypes = ("#include <hn/lib.hpp>\n\n" ++) . joinStr "\n" . map show . compileDefinition2 libraryTypes 

compileDefinition2 libraryTypes self = AG.compile2 self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = libraryTypes
	}

