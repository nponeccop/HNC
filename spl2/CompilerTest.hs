module CompilerTest where
import Test.HUnit hiding (test)

import HN.Parser2
import Utils
import CPP.CompileTools

compile inFile f = parseFile inFile >>= return . f . head . fromRight

compileFile inFile
	= compile inFile $ show . compileDefinition

test testName = do
	x <- compileFile $ "hn_tests/" ++ testName ++ ".hn"
	y <- readFile $ "hn_tests/" ++ testName ++ ".cpp"
	return $ TestCase $ assertEqual testName x y

iotests = map test [
		"00"
	]
