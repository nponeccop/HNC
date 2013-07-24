module Main (main) where

import Control.Applicative
import System.Environment
import System.Console.GetOpt

import CPP.CompileTools
import FFI.TypeParser
import HN.Optimizer.Frontend
import HN.SplExport (convertToSpl)
import HN.Visualise (formatHN)

compileWithOpt inFile libraryTypes 
	= compileHN libraryTypes <$> optimizeHN libraryTypes <$> parseHN inFile

dumpOpt inFile libraryTypes 
	=  formatHN <$> optimizeHN libraryTypes <$> parseHN inFile

data Flag = Spl | Optimize | DumpOpt | Help | Import String

options	=
	[ Option ['O'] [] (NoArg Optimize) "optimize using HOOPL"
	, Option [] ["spl"] (NoArg Spl) "output SPL.Types.C and SPL sources to stdout"
	, Option "i" ["hni"] (ReqArg Import "FILE.hni") "import FILE.hni instead of default lib.hni"
	, Option [] ["dump-opt"] (NoArg DumpOpt) "dump optimized HN to stdout"
	, Option "h?" ["help"] (NoArg Help)  "show this help"
	]

compilerOpts argv =
	case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ help))

help = usageInfo header options where
	header = "Usage: hnc <infile> [<outfile> | --spl | --types | -O ]\n"

main = getArgs >>= compilerOpts >>= g where
	g ([DumpOpt], [inFile]) 
		= importHni "lib/lib.hni" >>= dumpOpt inFile >>= putStr
	g ([Import hniFileName], [inFile]) 
		= importHni hniFileName >>= compileFile inFile >>= putStr
	g ([], [inFile]) 
		= importHni "lib/lib.hni" >>= compileFile inFile >>= putStr
	g ([], [inFile, outFile]) 
		= importHni "lib/lib.hni" >>= compileFile inFile >>= writeFile outFile 
	g ([Optimize], [inFile]) 
		= importHni "lib/lib.hni" >>= compileWithOpt inFile >>= putStr
	g ([Spl], [inFile]) = convertToSpl <$> parseHN inFile >>= putStr
	g ([Help], _) = putStrLn help
	g ([], []) = putStrLn help
	g (_, _) = error "Unrecognized command line"
