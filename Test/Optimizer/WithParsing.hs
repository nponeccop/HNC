module Test.Optimizer.WithParsing (tests, main) where
import qualified Data.Map as M
import Test.HUnit hiding (test)

import HN.Parser2
import HN.Optimizer.WithGraph

fakeLib :: M.Map String a
fakeLib = M.fromList $ zip ["print", "sub", "mul", "natrec", "sum", "incr"] $ repeat $ error "Test.Optimizer.WithParsing.fakeValue"

realTest inFile = TestCase $ do
        x <- parseAndProcessFile inFile id
        x @=? withGraph fakeLib id x

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
