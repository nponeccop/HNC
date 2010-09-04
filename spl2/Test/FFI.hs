module Test.FFI where

import FFI.Visualise
import SPL.Top
import Test.HUnit
import Utils

mk s e = TestLabel s $ TestCase $ assertEqual "" e $ showType $ uncondLookup s get_types

tests =
	[
		mk "sum" "num num -> num"
	,	mk "bind" "IO<a> (a -> IO<b>) -> IO<b>"
	]
