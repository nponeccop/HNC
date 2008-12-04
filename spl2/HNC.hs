module Main where

import qualified Data.Map as M

import HN.Parser2
import CPP.Core
import Utils
import CPP.Intermediate
import SPL.Top
import System
	
tdi2 = DefinitionInherited {
	diLevel        = 0,
	diSymTab       = M.map (const $ CppFqMethod "ff") SPL.Top.get_types,
	diFreeVarTypes = SPL.Top.get_types
,	diType         = Nothing
}

compileFile inFile = parseFile inFile >>= return . (++) "#include <hn/lib.hpp>\n\n" . show . dsCppDef . sem_Definition tdi2 . head . fromRight

main = getArgs >>= f where
	f [inFile, outFile] = compileFile inFile >>= writeFile outFile
	f [inFile] = compileFile inFile >>= putStr
	f _ = putStrLn "Usage: hnc <infile> [<outfile>]\n"
 