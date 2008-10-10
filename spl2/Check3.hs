
module Check3 (P (..), check0, res) where

import Data.Map as M hiding (filter)

import Types
import Code hiding (res)
import Top
import Hugs.Observe
--observe a b = b

data P = P (Map [Char] T, T) | N [Char]
	deriving Show


get_r (P (ur, r)) = r
get_rl l = Prelude.map get_r l

union a b =
	M.unionWith (\a b ->
		case Check3.compare a b of
			(_, _, True) -> a -- b ?
			(_, _, False) -> error "union"
	) a b

ch [] [] et ul uv i =
	N "too many parameters"
ch (r:[]) [] et ul uv i =
	P (uv, setm r ul)
ch r [] et ul uv i =
	P (uv, setm (TT r) ul)
ch (r:rs) (p1:ps) et ul uv i =
	case check p1 et of
		P (rm, r_p1) ->
			let r_p2 = change_tu r_p1 i in
			let rm2 = M.map (\x -> change_tu x i) rm in
			case Check3.compare (observe "cmp1" (setm r ul)) (observe "cmp2" r_p2) of
				(l2, r2, True) ->
					let lu = M.union l2 ul
					in let ru_ = M.union (M.union (observe "uv" r2) uv) rm2
					in let ru = M.map (\x -> setm x ru_) ru_
					in ch rs ps et lu ru (i+1)
				(l2, r2, False) ->
					N ("expected "++show (setm r ul)++", actual "++show r_p1)
		N e -> N e

check::C -> Map [Char] T -> P
check (CNum n) et = P (M.empty, T "num")
check (CBool n) et = P (M.empty, T "boolean")
check (CStr n) et = P (M.empty, T "string")
check (CVal n) et =
	case M.lookup n et of
		Just a -> P (M.empty, a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K [])) et =
	check a et

check (CL a (K p)) et =
	case check a et of
		P (rm0, TT r) ->
			case ch r p et M.empty M.empty 0 of
				P (rm, r) ->
					P (Check3.union rm0 rm, r)
				N e -> N (e++" for "++show a)
--		P (rm, TU n) ->
--			P (putp [n] [TT ((get_rl p_ok)++[TU ('_':n)])] rm, TU ('_':n))
		P (_, TT []) ->
			N ("too many parameters for "++show a)
		P (ur, TU n) ->
			P (putp [n] [TT (get_rl p_ok++[TU ('_':n)])] M.empty, TU ('_':n)) -- ?
		P (ur, TV n) ->
			P (putp [n] [TT (get_rl p_ok++[TU ('_':n)])] M.empty, TU ('_':n)) -- ?
		N e -> N e
	where
		p_ok = Prelude.map (\x -> check x et) p

check (CL a (S [])) et =
	check a et

check (CL a (S (p:ps))) et =
	case check (CL a (S ps)) (putp [p] [TV p_n] et) of
		P (ur, r) ->
			case M.lookup (p_n) (observe "a" ur) of
				Just v ->
					let w = case (v, r) of
						(a, TT b) -> TT (a:b)
						(a, b) -> TT [a, b]
					in
					P (ur, w)
				Nothing ->
					let w = case r of
						TT b -> TT ((TU p_n):b)
						b -> TT [TU p_n, b]
					in
					P (ur, w) -- rm ?
		o -> o
	where p_n = ""++p

check (CL a L) et =
	case check a et of
		P (ur, r) ->
			P (ur, TT [TL, r])
		o -> o
	
check (CL a R) et =
	case check a (putp ["_f"] [TU "_f"] et) of
		P (ur, r) -> check a (putp ["_f"] [r] et)
		o -> o

check o et =
	error ("check o: "++show o)

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et
putp o1 o2 et = error ("Check3.putp: "++show o1++", "++show o2)

compare (T a) (T b)|a == b = (M.empty, M.empty, True)
compare (TD a l1) (TD b l2)|a == b = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check3.compare l1 l2
--compare (TT l1) (TT l2) = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check3.compare l1 l2
-- error: TT [T "num",TU "_l"]/TT [TU "_",TU "z",T "num"]
compare (TT []) (TT []) =
	(M.empty, M.empty, True)
compare (TT [TU a]) b@(TT l)|1 < length l =
	(M.singleton a b, M.empty, True)
compare (TT (l1:l1s)) (TT (l2:l2s)) =
	(M.union l ll, M.union r rr, b && bb)
	where
		(l, r, b) = Check3.compare l1 l2
		(ll, rr, bb) = Check3.compare (TT l1s) (TT l2s)
compare a (TV n) = (M.empty, M.singleton n a, True)
compare (TU a) (TU b) = (M.empty, M.empty, True)
compare (TU n) b = (M.singleton n b, M.empty, True)
compare a (TU n) = (M.empty, M.singleton n a, True) -- correct ?
compare TL TL = (M.empty, M.empty, True) -- return lazy?
--compare t1 t2 = error $ (show t1)++"/"++(show t2)
compare t1 t2 = (M.empty, M.empty, False)

setu (TD n tt) u = TD n (Prelude.map (\t -> setu t u) tt)
setu (TT tt) u = TT (Prelude.map (\t -> setu t u) tt)
setu (TU n) (t2:t2s) = t2
setu o (t2:t2s) = o
setu o [] = o

setml2 l u = Prelude.map (\(P (rm, r)) -> P (rm, setm r u)) l
setml l u = Prelude.map (\x -> setm x u) l
setm (TD n tt) u = TD n (Prelude.map (\t -> setm t u) tt)
setm (TT tt) u = TT (Prelude.map (\t -> setm t u) tt)
setm (TU n) u =
	case M.lookup n u of
		Just a -> a
		Nothing -> TU n
setm o u = o

change_tul tt i = Prelude.map (\t -> change_tu t i) tt
change_tu (TT tt) i = TT $ change_tul tt i
change_tu (TD n tt) i = TD n $ change_tul tt i
change_tu (TU n) i = TU (n++show i)
change_tu o i = o

check0 o =
	observe "ret" $ check o Top.get_types

res = Check3.compare (TD "list" [TT [T "num",T "num"]]) (TD "list" [TT [T "num",T "num"]])



