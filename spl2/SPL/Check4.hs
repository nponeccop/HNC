module Main where

import SPL.Types
import qualified Data.Map as M

--_in = CL (CVal "sum") (K [CL (CVal "sum") (K [CNum 1, CNum 2])])
--_in = CL (CVal "sum") (K [CNum 1, CNum 2])
--_in = CL (CL (CVal "flip") (K [CVal "sum"])) (W [("flip",CL (CL (CVal "f") (K [CVal "x",CVal "y"])) (S ["f","x","y"]))])
--_in = (CL (CNum 1) (S ["a", "b"]))
--_in = CL (CL (CVal "f") (K [CVal "x", CVal "y"])) (S ["f", "x", "y"])
_in = CL (CVal "f") (S ["f"])


check::C -> (T, M.Map [Char] T)
check (CNum n) =
	(T "num", M.empty)
check (CVal n) =
	(TU rt, M.singleton n $ TU rt)
	where rt = '_':n
check (CL f (K ps)) =
	case check f of
		(tu, r) -> ch_k tu r ps
--		(o, _) -> error ("check K: "++show o)
check (CL f (S ss)) =
	case check f of
		(tu, r) -> ch_s tu r ss
--		(o, _) -> error ("check S: "++show o)
check (CL f (W [])) =
	check f
check (CL f (W ((s, p):ws))) =
	check (CL (CL (CL f (S [s])) (K [p])) (W ws))
check o = error ("ch:"++show o)

ch_s t r [] =
	(t, r)
ch_s t r (s:ss) =
	ch_s (TT [TU v_n, t]) (M.insert v_n (TU ("n_"++s)) r) ss
	where v_n = "v_"++s

ch_k t r [] =
	(t, r)
ch_k t r (p:ps) =
	case check p of
		(tu, r2) -> ch_k t (M.union M.empty $ M.map (\f ->
					map_type f t (TT [tu, t])
			) r) ps
--		(o2, r2) -> error ("check K.1: "++show o2++"|"++show r2)

map_type (TT []) b n = TT []
map_type a b n| a == b = n
map_type (TT (a:as)) b n =
	case map_type (TT as) b n of
		TT nn -> TT ((map_type a b n):nn)
		_ -> error "map_type"
map_type a b n = a

res = show $ check _in

main = putStrLn res

