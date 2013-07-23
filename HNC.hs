module Main (main) where

import System.Environment
import System.Console.GetOpt

import CPP.CompileTools

import HN.Optimizer.Frontend
import HN.SplExport

import SPL.Visualise
import Utils
import Control.Applicative
import qualified Data.Map as M
import FFI.TypeParser
import HN.Parser2
import HN.Visualise (showD)

compileFile = compile2 id

compileWithOpt symbolList = compile2 (map (optimize symbolList))

dumpOpt inFile = do
	symbolList <- importHni "lib/lib.hni" 
	joinStr "\n" . map (showD . optimize (M.keys symbolList)) . fromRight <$> parseFile inFile   

compileToSpl inFile = do
	x <- compile inFile (map convertDef)
	return $ show x ++ "\n" ++ joinStr "\n" (map showAsSource x)

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
	g ([DumpOpt], [inFile]) = dumpOpt inFile >>= putStr
	g ([Import i], [inFile]) = compile3 i id inFile >>= putStr
	g ([], [inFile]) = compileFile inFile >>= putStr
	g ([], [inFile, outFile]) = compileFile inFile >>= writeFile outFile
	g ([Spl], [inFile]) = compileToSpl inFile >>= putStr
	g ([Optimize], [inFile]) = do
		symbolList <- fmap M.keys $ importHni "lib/lib.hni" 
		compileWithOpt symbolList inFile >>= putStr
	g ([Help], _) = putStrLn help
	g ([], []) = putStrLn help
	g (_, _) = error "Unrecognized command line"
