module Main where

import qualified Data.Map as M
import System.Environment

import Utils

import CPP.CompileTools

import HN.Parser2
import HN.SplExport
import HN.TypeParser

import SPL.Check3
import SPL.Visualise


compile inFile f = parseFile inFile >>= return . f . head . fromRight

compileFile inFile = do
	a <- readFile "lib/lib.hni" >>= return . M.fromList . map (sp3 decl) . lines
	compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . show . compileDefinition2 a

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
	f [inFile, outFile] = compileFile inFile >>= writeFile outFile
	f [inFile] = compileFile inFile >>= putStr
	f _ = putStrLn "Usage: hnc <infile> [<outfile> | --spl | --types ]\n"
