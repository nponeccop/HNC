module Main where

import qualified Data.Map as M
import System.Environment
import System.Console.GetOpt

import Utils

import CPP.CompileTools

import HN.Optimizer.Frontend
import HN.Parser2
import HN.SplExport
import HN.TypeParser

import SPL.Check3
import SPL.Visualise

compileFile = compile2 id

compileWithOpt = compile2 optimize

typeCheck inFile = compile inFile typecheckDefinition

dumpInferredTypes (P (x, _, _)) = do
	putStrLn $ showTypedTreeWithoutTypes x
	putStrLn ""
	putStrLn $ showTypedTree x
	putStrLn ""
	print x

dumpInferredTypes (N _ y) = error y

compileToSpl inFile = do
	x <- compile inFile convertDef
	return $ show x ++ "\n" ++ showAsSource x

data Flag = Spl | Types | Optimize | Help

options	=
	[ Option ['O'] [] (NoArg Optimize) "optimize using HOOPL"
	, Option [] ["spl"] (NoArg Spl) "output SPL.Types.C and SPL sources to stdout"
	, Option [] ["types"] (NoArg Types) "typecheck using older SPL typechecker"
	, Option "h?" ["help"] (NoArg Help)  "show this help"
	]

compilerOpts argv =
	case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ help))


help = usageInfo header options where
	header = "Usage: hnc <infile> [<outfile> | --spl | --types | -O ]\n"

main = getArgs >>= compilerOpts >>= g where
	g ([], [inFile]) = compileFile inFile >>= putStr
	g ([], [inFile, outFile]) = compileFile inFile >>= writeFile outFile
	g ([Spl], [inFile]) = compileToSpl inFile >>= putStr
	g ([Types], [inFile]) = typeCheck inFile >>= dumpInferredTypes
	g ([Optimize], [inFile]) = compileWithOpt inFile >>= putStr
	g ([Help], _) = putStrLn help
	g (o, n) = error "Unrecognized command line"
