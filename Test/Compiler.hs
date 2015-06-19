module Test.Compiler (iotests) where
import Test.HUnit hiding (test)
import System.Directory
import Control.Monad
import System.IO
import CPP.CompileTools
import FFI.TypeParser (importHni)

windowsLineMode = NewlineMode { inputNL  = CRLF, outputNL = CRLF }

readFileCRLF name = do
	h <- openFile name ReadMode
 	hSetNewlineMode h windowsLineMode
	hGetContents h

comp2 f g x y = f $ g x y

test testName =	liftM2 (comp2 (testName ~:) (~=?))
	(readFileCRLF $ "hn_tests/" ++ testName ++ ".cpp")
	(importHni "lib/lib.hni" >>= compileFile ("hn_tests/" ++ testName ++ ".hn"))

iotests =
	(map fst . filter (\x -> snd x == ".hn") . map (break (== '.'))) <$> getDirectoryContents "hn_tests" 
	>>=	mapM test



