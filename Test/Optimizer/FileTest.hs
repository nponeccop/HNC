module Test.Optimizer.FileTest (iotests) where
import Test.HUnit hiding (test)
import System.Directory
import Control.Monad
import System.IO
import CPP.CompileTools
import FFI.TypeParser (importHni)

import HN.Optimizer.Frontend
import HN.Visualise (formatHN)

windowsLineMode = NewlineMode { inputNL  = CRLF, outputNL = CRLF }

readFileCRLF name = do
	h <- openFile name ReadMode
 	hSetNewlineMode h windowsLineMode
	hGetContents h

comp2 f g x y = f $ g x y

dumpOpt inFile libraryTypes 
	=  formatHN <$> optimizeHN libraryTypes <$> parseHN inFile


test testName =	liftM2 (comp2 ((testName ++ "_opt") ~:) (~=?))
	(readFileCRLF $ "hn_tests/opt/" ++ testName ++ ".hn")
	(importHni "lib/lib.hni" >>= dumpOpt ("hn_tests/" ++ testName ++ ".hn"))

iotests =
	(map fst . filter (\x -> snd x == ".hn") . map (break (== '.'))) <$> getDirectoryContents "hn_tests/opt" >>=
	mapM test
