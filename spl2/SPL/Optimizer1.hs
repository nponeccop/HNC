module SPL.Optimizer1 where

import SPL.Types
import SPL.Check3 hiding (res)

_in = CTyped (T "num") (CL (CL (CTyped (TU "a") (CL (CL (CVal "a") (S ["y"])) (K [CTyped (TU "a") (CVal "a")]))) (S ["a"])) (K [CTyped (T "num") (CNum 5)]))

--Spli.hs: optimizer error: CL (CL (CTyped (TV "a") (CL (CL (CTyped (TV "b") (CL (CL (CVal "c") (S ["c"])) (K [CTyped (TU "b") (CVal "b")]))) (S ["b"])) (K [CTyped (T "num") (CVal "a")]))) (S ["a"])) (K [CTyped (T "num") (CNum 5)])
 
opt (CTyped t c) =
	CTyped t $ opt c

opt (CL (CL c (S [s])) (K [CTyped t v])) =
	CL (CL (opt $ opt_val c t s) (S [s])) (K [CTyped t v])

opt (CVal v) =
	CVal v

opt o =
	error ("optimizer error: "++show o)

--Spli.hs: CTyped (TV "a") (CL (CL (CTyped (TV "b") (CL (CL (CVal "c") (S ["c"])) (K [CTyped (TU "b") (CVal "b")]))) (S ["b"])) (K [CTyped (TU "a") (CVal "a")]))

opt_val (CTyped t2 (CVal v)) t s|v == s =
	CTyped t (CVal v)

opt_val (CTyped t2 c) t s =
	CTyped t2 $ opt_val c t s

opt_val (CL c (K ps)) t s =
	CL c (K $ map (\x -> opt_val x t s) ps)

opt_val (CVal v) t s =
	CVal v

opt_val c t s =
	error $ show c

res = opt _in

main = putStrLn $ (show _in)++"\n"++(show res)




