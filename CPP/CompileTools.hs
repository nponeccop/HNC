module CPP.CompileTools (parseHN, compileHN, compileFile, extractLean) where

import Text.Parsec.ByteString (parseFromFile)

import qualified Bar as AG
import HN.Parser2 (program)
import Utils

compileFile inFile libraryTypes = compileHN libraryTypes <$> parseHN inFile

parseHN inFile  = fromRight <$> parseFromFile program inFile

compileHN libraryTypes = ("#include <hn/lib.hpp>\n\n" ++) . joinStr "\n" . map show . compileDefinition2 libraryTypes 

compileDefinition2 libraryTypes self = AG.compile2 self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = libraryTypes
	}

extractLean' libraryTypes self = AG.extractLean self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = libraryTypes
	}

extractLean inFile libraryTypes = extractLean' libraryTypes <$> parseHN inFile

