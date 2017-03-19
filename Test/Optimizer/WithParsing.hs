module Test.Optimizer.WithParsing (tests, main) where
import qualified Data.Map as M
import Test.HUnit hiding (test)
import Text.Parsec.ByteString (parseFromFile)

import HN.Optimizer.Frontend (withGraph)
import Parser.Parser
import Utils (fromRight)

fakeLib :: M.Map String a
fakeLib = M.fromList $ zip ["print", "sub", "mul", "natrec", "sum", "incr"] $ repeat $ error "Test.Optimizer.WithParsing.fakeValue"

parseAndProcessFile inFile f = (f . head . fromRight) <$> parseFromFile program inFile

realTest inFile = TestCase $ do
	x <- parseAndProcessFile inFile id
	x @=? withGraph fakeLib (const id) x

tests = "WithParsing" ~:  map (\x -> realTest ("hn_tests/" ++ x ++ ".hn"))
	[
	-- "locals14"
	 "where2"
	, "where"
	, "plusX"
	--, "locals2"
	--, "euler6"
	]

main = runTestTT tests
