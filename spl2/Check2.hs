
module Check2 (P (..), check0, res) where

import Data.Map as M hiding (filter)

import Types
import Code hiding (res)
import Top
import Debug.Trace

data P = P (Map [Char] T, T) | N [Char]
	deriving Show


get_r (P (ur, r)) = r
get_rl l = Prelude.map get_r l

union a b =
	M.unionWith (\a b ->
		case Check2.compare a b of
			(_, _, True) -> a -- b ?
			(_, _, False) -> error "union"
	) a b

ch (r:[]) [] et ur uv =
--	trace ("cmpRet: "++show r++" |"++show ur) $
	P (ur, setm r ur)
ch r [] et ur uv =
	P (ur, setm (TT r) ur)
ch (r:rs) (p1:ps) et ur uv =
	case p1 of
		P (rm, r_p1) ->
			case Check2.compare r r_p1 of
				(l2, r2, True) ->
--					trace ("cmp: "++show rm++"|"++show l2++"|"++show ur) $
					ch rs ps et $ Check2.union ur l2
				(l2, r2, False) -> N ("expected "++show (setm r ur)++", actual "++show r_p1)
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
		P (rm, TT r) ->
			ch r p_ok et M.empty
		P (rm, TU n) ->
			P (putp [n] [TT ((get_rl p_ok)++[TU ('_':n)])] rm, TU ('_':n))
		N e -> N e
	where
		p_ok = Prelude.map (\x -> check x et) p

check (CL a (S [])) et =
	check a et

check (CL a (S (p:ps))) et =
	case check (CL a (S ps)) (putp [p] [TV p] et) of
		P (ur, r) ->
			case M.lookup p ur of
				Just v -> P (ur, TT [v, setm r ur]) -- rm ?
				Nothing -> P (ur, TT [TV p, setm r ur]) -- rm ?
		o -> o
	
putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et
putp o1 o2 et = error ("Check2.putp: "++show o1++", "++show o2)

compare (T a) (T b)|a == b = (M.empty, M.empty, True)
compare (TD a l1) (TD b l2)|a == b = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check2.compare l1 l2
compare (TT l1) (TT l2) = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check2.compare l1 l2
compare (TV n) b = (M.empty, M.empty, True)
compare a (TV n) = (M.empty, M.empty, True)
compare (TU n) b = (M.singleton n b, M.singleton n b, True)
compare a (TU n) = (M.singleton n a, M.singleton n a, True)
compare TL TL = (M.empty, M.empty, True) -- return lazy?
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

check0 o =
--	trace ("check0: "++show o) $
	check o Top.get_types

res = Check2.compare (TD "list" [TT [T "num",T "num"]]) (TD "list" [TT [T "num",T "num"]])



