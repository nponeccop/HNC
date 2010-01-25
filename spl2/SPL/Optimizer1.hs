module Optimizer1 where

import SPL.Types

_in = CTyped (T "num") (CL (CL (CTyped (TU "a") (CL (CL (CVal "a") (S ["y"])) (K [CTyped (TU "a") (CVal "a")]))) (S ["a"])) (K [CTyped (T "num") (CNum 5)]))

opt (CTyped t c) =
	CTyped t $ opt c
opt (CL (CL c (S (s:[]))) (K (k:[]))) =
	CL (CL (opt_val c s) (S (s:[]))) (K (k:[]))
opt o =
	error ("Optimizer: "++show o)

opt_val (CTyped t c) s =
	CTyped t (opt_val c s)
opt_val c s = c
	--error $ show c

res = opt _in

main = putStrLn $ show res

