module Test.MilnerTools (tests) where
import HN.MilnerTools
import Test.HUnit
import SPL.Visualise
import SPL.Top
import Utils

libType name = uncondLookup name SPL.Top.get_types

instantiatedTypeTest t e = e ~: e ~=? makeType (snd $ instantiatedType (libType t) 10)

tests = "MilnerTools.instantiatedType" ~: [
		instantiatedTypeTest "print" "?t10 -> IO void"
	,	instantiatedTypeTest "bind" "IO ?t10 -> (?t10 -> IO ?t11) -> IO ?t11"
	]
