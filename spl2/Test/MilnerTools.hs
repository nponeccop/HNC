module Test.MilnerTools where
import MilnerTools
import Test.HUnit
import SPL.Visualise
import SPL.Top
import Utils

libType name = uncondLookup name SPL.Top.get_types

instantiatedTypeTest t e = TestLabel "instantiatedTypeTest" $ TestCase $ assertEqual "" e  $ makeType $ snd $ instantiatedType (libType t) 10

instantiatedTypeTests = [
		instantiatedTypeTest "print" "?t10 -> IO void"
	,	instantiatedTypeTest "bind" "IO ?t10 -> (?t10 -> IO ?t11) -> IO ?t11"
	]
