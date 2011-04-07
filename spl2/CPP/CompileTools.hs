module CPP.CompileTools (compileDefinition2, compile2, compile) where

import qualified Bar as AG
import HN.Parser2
import HN.TypeParser
import Utils

import qualified Data.Map as M

compile inFile f = parseFile inFile >>= return . f . head . fromRight

compile2 f inFile = do
	a <- readFile "lib/lib.hni" >>= return . M.fromList . map (sp3 decl) . lines
	compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . show . compileDefinition2 a . f

compileDefinition2 libraryTypes self = AG.compile2 self inh where
	inh = AG.Inh_Root {
		AG.library_Inh_Root = libraryTypes
	}

