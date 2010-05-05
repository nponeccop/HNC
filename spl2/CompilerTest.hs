module CompilerTest where
import Test.HUnit hiding (test)
import System.Directory
import Data.String
import Control.Monad

import HN.Parser2
import Utils
import CPP.CompileTools

compile inFile f = parseFile inFile >>= return . f . head . fromRight

compileFile inFile
	= compile inFile $ (++) "#include <hn/lib.hpp>\n\n" . show . compileDefinition

comp2 f g x y = f $ g x y

test testName =	liftM2
	(comp2 TestCase $ assertEqual testName)
	(compileFile $ "hn_tests/" ++ testName ++ ".hn")
	(readFile $ "hn_tests/" ++ testName ++ ".cpp")


iotests =
	getDirectoryContents "hn_tests" >>=
	return . map fst . filter (\x -> snd x == ".hn") . map (break (== '.')) >>=
	mapM test



