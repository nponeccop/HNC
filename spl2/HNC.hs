module Main (main) where

import System.Environment
import System.Console.GetOpt

import CPP.CompileTools

import HN.Optimizer.Frontend
import HN.SplExport

import SPL.Check3
import SPL.Visualise
import SPL.Top

compileFile = compile2 id

compileWithOpt = error "compileWithOpt" -- compile2 optimize

typeCheck inFile = error "typeCheck" where -- compile inFile typecheckDefinition where
	typecheckDefinition self = check1 (convertDef self) SPL.Top.get_types

dumpInferredTypes (P (x, _, _)) = do
	putStrLn $ showTypedTreeWithoutTypes x
	putStrLn ""
	putStrLn $ showTypedTree x
	putStrLn ""
	print x

dumpInferredTypes (N _ y) = error y

compileToSpl inFile = do
	x <- error "compileToSpl" -- compile inFile convertDef
	return $ show x ++ "\n" ++ showAsSource x

data Flag = Spl | Types | Optimize | Help | Import String

options	=
	[ Option ['O'] [] (NoArg Optimize) "optimize using HOOPL"
	, Option [] ["spl"] (NoArg Spl) "output SPL.Types.C and SPL sources to stdout"
	, Option [] ["types"] (NoArg Types) "typecheck using older SPL typechecker"
	, Option "i" ["hni"] (ReqArg Import "FILE.hni") "import FILE.hni instead of default lib.hni"
	, Option "h?" ["help"] (NoArg Help)  "show this help"
	]

compilerOpts argv =
	case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ help))


help = usageInfo header options where
	header = "Usage: hnc <infile> [<outfile> | --spl | --types | -O ]\n"

main = getArgs >>= compilerOpts >>= g where
	g ([Import i], [inFile]) = compile3 i id inFile >>= putStr
	g ([], [inFile]) = compileFile inFile >>= putStr
	g ([], [inFile, outFile]) = compileFile inFile >>= writeFile outFile
	g ([Spl], [inFile]) = compileToSpl inFile >>= putStr
	g ([Types], [inFile]) = typeCheck inFile >>= dumpInferredTypes
	g ([Optimize], [inFile]) = compileWithOpt inFile >>= putStr
	g ([Help], _) = putStrLn help
	g ([], []) = putStrLn help
	g (_, _) = error "Unrecognized command line"
