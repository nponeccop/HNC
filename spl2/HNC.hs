module Main where

import qualified Data.Map as M
import System.Environment

import Utils

import CPP.CompileTools
import HN.Parser2
import HN.SplExport
import HN.Intermediate

import SPL.Types
import SPL.Check3
import SPL.Visualise


compile inFile f = parseFile inFile >>= return . f . head . fromRight

compileFile inFile
	= compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . show . compileDefinition

typeCheck inFile = compile inFile typecheckDefinition


print2 (P (x, _, _)) = do
	putStrLn $ printFF False x
	putStrLn ""
	putStrLn $ printFF True x
	putStrLn ""
	print x

makeType (T x) = x
makeType (TD a b) =  a ++ " " ++ (joinStr " " $ map makeType b)
makeType (TT x) = joinStr " -> " $ map makeType2 x
makeType (TU x) = '?' : x
makeType x = show x

makeType2 (x @ (TT _)) = "(" ++ makeType x ++ ")"
makeType2 x = makeType x

printFF a (CTyped x y) = (if a then "[" ++ makeType x ++ "] " else "" ) ++  printFF a y
printFF a (CL x (K y)) = "(" ++ printFF a x ++ " " ++ (joinStr " " $ map (printFF a) y) ++ ")"
printFF a (CL x (S y)) =  "(\\" ++ concatMap (++ " ") y ++ "-> " ++ printFF2 a x ++ ")"
printFF _ (CVal x) = x
printFF _ (CNum x) = show x
printFF _ x = show x

printFF2 a (CL x (K y)) = printFF a x ++ " " ++ (joinStr " " $ map (printFF a) y)
printFF2 a (CTyped x y) = if a then "[" ++ makeType x ++ "] " ++ printFF a y else printFF2 a y
printFF2 a b = printFF a b

compileToSpl inFile = do
	x <- compile inFile convertDef
	return $ show x ++ "\n" ++ showAsSource x

main = getArgs >>= f where
	f [inFile, "--spl"] = compileToSpl inFile >>= putStr
	f [inFile, "--types"] = typeCheck inFile >>= print2
	f [inFile, outFile] = compileFile inFile >>= writeFile outFile
	f [inFile] = compileFile inFile >>= putStr
	f _ = putStrLn "Usage: hnc <infile> [<outfile> | --spl | --trace-p | --types ]\n"
