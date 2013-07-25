module Main (main) where

import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M
import System.Console.GetOpt

import CPP.CompileTools
import FFI.TypeParser
import HN.Optimizer.Frontend
import HN.SplExport (convertToSpl)
import HN.Visualise (formatHN)
import Utils.Options

compileWithOpt inFile libraryTypes 
	= compileHN libraryTypes <$> optimizeHN libraryTypes <$> parseHN inFile

dumpOpt inFile libraryTypes 
	=  formatHN <$> optimizeHN libraryTypes <$> parseHN inFile

options	=
	[ Option ['O'] [] (NoArg $ OptBool "O") "optimize using HOOPL"
	, Option [] ["spl"] (NoArg $ OptBool "spl") "output SPL.Types.C and SPL sources"
	, Option "i" ["hni"] (ReqArg (OptString "i") "FILE.hni") "import FILE.hni instead of default lib.hni"
	, Option [] ["dump-opt"] (NoArg $ OptBool "dump-opt") "dump optimized HN"
	, Option "h?" ["help"] (NoArg $ OptBool "help")  "show this help"
	]
	
help = usageInfo header options where
	header = "Usage: hnc [options] <infile> [<outfile>]\n"

main = runOptions options ff

ff (O oBool oString oNonOptions) = f where  
	b x = S.member x oBool
	hni = importHni $ M.findWithDefault "lib/lib.hni" "hni" oString
	output = if (length oNonOptions == 2) then writeFile outFile else putStr
	outFile = oNonOptions !! 1
	inFile = oNonOptions !! 0
	dumpOptFlag = b "dump-opt"
	splFlag = b "spl"
	optFlag = b "O"
	helpFlag = b "help"
	processWith f = hni >>= f inFile >>= output
	f 	| length oNonOptions > 2 = err "Too many files specified in command line"
		| length oNonOptions == 0 || helpFlag = putStrLn help
		| dumpOptFlag = processWith dumpOpt
		| optFlag = processWith compileWithOpt
		| splFlag = convertToSpl <$> parseHN inFile >>= output
		| otherwise = processWith compileFile
