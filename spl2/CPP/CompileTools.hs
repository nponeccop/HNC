module CPP.CompileTools (compileDefinition2, compile2, compile) where

import qualified Bar as AG
import HN.Parser2
import FFI.TypeParser
import Utils

import qualified Data.Map as M

compile inFile f = parseFile inFile >>= return . f . fromRight

compile2 f inFile = do
	libraryTypes <- readFile "lib/lib.hni" >>= return . M.fromList . map parseDecl . lines
	compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . joinStr "\n" . map show . compileDefinition2 libraryTypes . f

compileDefinition2 libraryTypes self = AG.compile2 self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = libraryTypes
	}

