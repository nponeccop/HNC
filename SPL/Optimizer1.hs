{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SPL.Optimizer1 where

import SPL.Types
import qualified Data.Map as M

_in = CTyped (T "num") (CL (CL (CTyped (TU "a") (CL (CL (CVal "a") (S ["y"])) (K [CTyped (TU "a") (CVal "a")]))) (S ["a"])) (K [CTyped (T "num") (CNum 5)]))

opt (CDebug d c) = CDebug d $ opt c
opt (CNum n) = CNum n
opt (CBool b) = CBool b
opt (CStr s) = CStr s
opt (CStruct m) = CStruct $ M.map opt m
opt (CTyped t c) =
	CTyped t $ opt c
opt (CL (CL c (S [s])) (K [CTyped t v])) =
	CL (CL (opt $ opt_val c t s) (S [s])) (K [CTyped t v])
opt (CL c (K ps)) =
	CL (opt c) (K $ Prelude.map opt ps)
opt (CL c (S ss)) =
	CL (opt c) $ S ss
--opt (CL c (W =
opt (CL c (D n)) =
	CL (opt c) $ D n
opt (CL c R) =
	CL (opt c) R
opt (CL c L) =
	CL (opt c) L
opt (CVal v) =
	CVal v
opt o =
	error ("optimizer error: "++show o)

opt_val (CDebug d c) t s = CDebug d c
opt_val (CNum n) t s = CNum n
opt_val (CBool b) t s = CBool b
opt_val (CStr s2) t s = CStr s2
opt_val (CStruct m) t s = CStruct $ M.map (\x -> opt_val x t s) m
opt_val (CTyped t2 (CVal v)) t s|v == s =
	CTyped t (CVal v)
opt_val (CTyped t2 c) t s =
	CTyped t2 $ opt_val c t s
opt_val (CL c (K ps)) t s =
	CL c (K $ map (\x -> opt_val x t s) ps)
opt_val (CL c (S ss)) t s|elem s ss =
	CL c $ S ss
opt_val (CL c (S ss)) t s =
	CL (opt_val c t s) $ S ss
opt_val (CL c (D n)) t s =
	CL (opt_val c t s) $ D n
opt_val (CL c R) t s =
	CL (opt_val c t s) R
opt_val (CL c L) t s =
	CL (opt_val c t s) L

opt_val (CVal v) t s =
	CVal v

opt_val c t s =
	error ("optimizer error2:"++show c)

res = opt _in

main = putStrLn $ (show _in)++"\n"++(show res)




