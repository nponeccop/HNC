module Optimizer1 where

import SPL.Types
import SPL.Check3 hiding (res)

_in = CTyped (T "num") (CL (CL (CTyped (TU "a") (CL (CL (CVal "a") (S ["y"])) (K [CTyped (TU "a") (CVal "a")]))) (S ["a"])) (K [CTyped (T "num") (CNum 5)]))

opt (CTyped t c) =
	CTyped t $ opt c
opt (CL (CL c (S (s:[]))) (K ((CTyped t k):[]))) =
	CL (CL (opt_val c s t) (S (s:[]))) (K ((CTyped t k):[]))
opt o =
	error ("Optimizer: "++show o)

opt_val (CTyped t2 (CL c (K [CTyped (TU v1) (CVal v2)]))) s t =
	CTyped (opt_type t2 v1 t) $ CL c (K [CTyped t (CVal v2)])

opt_type (TU a) v1 t|a == v1 =
	t

res = opt _in

main = putStrLn $ (show _in)++"\n"++(show res)




