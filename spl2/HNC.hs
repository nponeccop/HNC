module Main where

import qualified Data.Map as M

import HN.Parser2
import HN.SplExport
import HN.Intermediate
import CPP.Core
import Utils
import CPP.Intermediate
import SPL.Top
import SPL.Types
import SPL.Check3
import SPL.Visualise
import System.Environment
import CPP.TypeProducer

tdi2 t types typed = DefinitionInherited {
	diLevel        = 0
,	diSymTab       = M.map (const $ CppFqMethod "ff") SPL.Top.get_types
,	diTraceP       = t
,	diRootTypes    = types
,	diTyped        = typed
}

compile inFile f = parseFile inFile >>= return . f . head . fromRight

compileFile t inFile
	= compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . show . dsCppDef . z
	where
		z self @ (Definition name _ _ _) = sem_Definition (tdi2 t types typed) self where
			P (typed, fv, x) = check1 (convertDef self) SPL.Top.get_types
			types = M.insert name x fv


typeCheck inFile = compile inFile f where
	f self = check1 (convertDef self) SPL.Top.get_types

print2 (P (x, _, _)) = do
	putStrLn $ printFF False x
	putStrLn ""
	putStrLn $ printFF True x
	putStrLn ""
	print x


makeType (T x) = x
makeType (TD a b) =  a ++ " " ++ (joinStr " " $ map makeType b)
makeType (TT x) = joinStr " -> " $ map makeType2 x
makeType (TU x) = "?" ++ x
makeType x = show x

makeType2 (x @ (TT _)) = "(" ++ (makeType x) ++ ")"
makeType2 x = makeType x

printFF a (CTyped x y) = (if a then "[" ++ makeType x ++ "] " else "" ) ++  printFF a y
printFF a (CL x (K y)) = "(" ++ (printFF a x) ++ " " ++ (concatMap (printFF a) y) ++ ")"
printFF a (CL x (S y)) =  "(\\" ++ (concatMap (\x -> x ++ " ") y) ++ "-> " ++ (printFF2 a x) ++ ")"
printFF _ (CVal x) = x
printFF _ (CNum x) = show x
printFF _ x = show x

printFF2 a (CL x (K y)) = (printFF a x) ++ " " ++ (concatMap (printFF a) y)
printFF2 a (CTyped x y) = if a then "[" ++ makeType x ++ "] " ++ printFF a y else printFF2 a y
printFF2 a b = printFF a b

compileToSpl inFile = do
	x <- compile inFile convertDef
	return $ show x ++ "\n" ++ showAsSource x

main = getArgs >>= f where
	f [inFile, "--spl"] = compileToSpl inFile >>= putStr
	f [inFile, "--trace-p"] = compileFile True inFile >>= putStr
	f [inFile, "--types"] = typeCheck inFile >>= print2
	f [inFile, outFile] = compileFile False inFile >>= writeFile outFile
	f [inFile] = compileFile False inFile >>= putStr
	f _ = putStrLn "Usage: hnc <infile> [<outfile> | --spl | --trace-p | --types ]\n"
