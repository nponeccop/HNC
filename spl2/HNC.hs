module Main where

import qualified Data.Map as M
import System.Environment

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

main = getArgs >>= f where
	f [inFile, "--spl"] = compileToSpl inFile >>= putStr
	f [inFile, "--types"] = typeCheck inFile >>= dumpInferredTypes
	f [inFile, "-O"] = compileWithOpt inFile >>= putStr
	f [inFile, outFile] = compileFile inFile >>= writeFile outFile
	f [inFile] = compileFile inFile >>= putStr
	f _ = putStrLn "Usage: hnc <infile> [<outfile> | --spl | --types | -O ]\n"
