module Main (main) where

import qualified Data.Set as S
import qualified Data.Map as M
import System.Console.GetOpt

import CPP.CompileTools
import FFI.TypeParser
import HN.Optimizer.Frontend
import HN.Optimizer.GraphCompiler
import HN.Optimizer.Visualise (formatGraph)
import HN.SplExport (convertToSpl)
import HN.Visualise (formatHN)
import Utils.Options

compileWithOpt inFile libraryTypes 
	= compileHN libraryTypes . optimizeHN libraryTypes <$> parseHN inFile

dumpOpt inFile libraryTypes 
	=  formatHN . optimizeHN libraryTypes <$> parseHN inFile

dumpGraph inFile libraryTypes
	= formatGraph . fst . compileGraph libraryTypes . head <$> parseHN inFile

options	=

	[ Option "O" [] (NoArg $ OptBool "O") "optimize using HOOPL"
	, Option [] ["spl"] (NoArg $ OptBool "spl") "output SPL.Types.C and SPL sources"
	, Option "i" ["hni"] (ReqArg (OptString "i") "FILE.hni") "import FILE.hni instead of default lib.hni"
	, Option [] ["dump-graph"] (NoArg $ OptBool "dump-graph") "dump HOOPL graph"
	, Option [] ["dump-opt"] (NoArg $ OptBool "dump-opt") "dump optimized HN"
	, Option [] ["dump-lean"] (NoArg $ OptBool "dump-lean") "dump Lean source"
	, Option "h?" ["help"] (NoArg $ OptBool "help")  "show this help"
	]
	
help = usageInfo header options where
	header = "Usage: hnc [options] <infile> [<outfile>]\n"

main = runOptions options ff

ff (O oBool oString oNonOptions) = f where  
	b x = S.member x oBool
	hni = importHni $ M.findWithDefault "lib/lib.hni" "hni" oString
	output = if length oNonOptions == 2 then writeFile outFile else putStr
	inFile = oNonOptions !! 0
	outFile = oNonOptions !! 1
	dumpOptFlag = b "dump-opt"
	dumpGraphFlag = b "dump-graph"
	splFlag = b "spl"
	optFlag = b "O"
	helpFlag = b "help"
	processWith f = hni >>= f inFile >>= output
	f 	| length oNonOptions > 2 = err "Too many files specified in command line"
		| null oNonOptions || helpFlag = putStrLn help
		| dumpGraphFlag = processWith dumpGraph
		| dumpOptFlag = processWith dumpOpt
		| optFlag = processWith compileWithOpt
		| splFlag = convertToSpl <$> parseHN inFile >>= output
		| b "dump-lean" = processWith extractLean
		| otherwise = processWith compileFile
