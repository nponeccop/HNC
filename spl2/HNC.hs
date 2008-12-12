module Main where

import qualified Data.Map as M

import HN.Parser2
import HN.SplExport
import HN.Intermediate
import CPP.Core
import Utils
import CPP.Intermediate
import SPL.Top
import SPL.Check3
import System
	
tdi2 t types = DefinitionInherited {
	diLevel        = 0,
	diSymTab       = M.map (const $ CppFqMethod "ff") SPL.Top.get_types,
	diFreeVarTypes = SPL.Top.get_types
,	diType         = Nothing
,	diTraceP       = t
,	diRootTypes	   = types
}

compile inFile f = parseFile inFile >>= return . f . head . fromRight

compileFile t inFile 
	= compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . show . dsCppDef . z 
	where
		z self @ (Definition name _ _ _) = sem_Definition (tdi2 t types) self where
			P (fv, x) = check1 (convertDef self) SPL.Top.get_types
			types = M.insert name x fv 


typeCheck inFile = compile inFile f where
	f self = check1 (convertDef self) SPL.Top.get_types

compileToSpl inFile = compile inFile convertDef

main = getArgs >>= f where
	f [inFile, "--spl"] = compileToSpl inFile >>= print
	f [inFile, "--trace-p"] = compileFile True inFile >>= putStr
	f [inFile, "--types"] = typeCheck inFile >>= print
	f [inFile, outFile] = compileFile False inFile >>= writeFile outFile
	f [inFile] = compileFile False inFile >>= putStr
	f _ = putStrLn "Usage: hnc <infile> [<outfile> | --spl | --trace-p ]\n"
