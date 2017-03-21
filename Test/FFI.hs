module Test.FFI where

import FFI.Visualise
import SPL.Top
import Utils

mk s e = st s e $ showType $ uncondLookup s get_types

tests =
	[
		mk "sum" "num num -> num"
	,	mk "bind" "IO<??a> (??a -> IO<??b>) -> IO<??b>"
	]
