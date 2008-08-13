
module Check (P (..), check0, res) where

import Data.Map as M hiding (filter)

import Types
import Code hiding (res)
import Top
import Debug.Trace

data P = P (Map [Char] T, T) | N [Char]
	deriving Show


get_r (P (ur, r)) = r

ch (r:[]) [] et ur =
--	trace ("ch: ret "++show r++" |"++show ur) $
	P (M.empty, r)
ch r [] et ur =
--	trace ("ch: rett "++show r++" |"++show ur) $
	P (M.empty, setm (TT r) ur)
ch (r:rs) (p1:ps) et ur =
--	trace ("ch: cmp "++show r++","++(show $ get_r p1)++" |"++show ur) $
	case p1 of
		P (rm, r_p1) ->
			case Check.compare r r_p1 of
				(l2, r2, True) -> ch (setml rs l2) (setml2 ps l2) et l2
				(l2, r2, False) -> N ("expected "++show r++", actual "++show r_p1)
		N e -> N e

check::C -> Map [Char] T -> P
check (CNum n) et = P (M.empty, T "num")
check (CBool n) et = P (M.empty, T "boolean")
check (CStr n) et = P (M.empty, T "string")
check (CVal n) et =
	case M.lookup n et of
		Just a -> P (M.empty, a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K p)) et =
	case check a et of
		P (rm, TT r) ->
			ch r ps et M.empty
		N e -> N e
	where
		ps = Prelude.map (\x -> check x et) p

check (CL a (S [])) et =
	trace ("checkS: "++(show $ check a et)) $
	check a et

check (CL a (S (p:ps))) et =
	trace ("checkS: "++show p) $
	case check (CL a (S ps)) (putp [p] [TU p] et) of
		P (ur, r) -> trace ("S: "++show ur) $
			case M.lookup p ur of
				Just v -> P (ur, TT [v, setm r ur]) -- rm ?
				Nothing -> P (ur, TT [TU p, setm r ur]) -- rm ?
		o -> o
	

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et
putp o1 o2 et = error ("Check.putp: "++show o1++", "++show o2)

compare (T a) (T b)|a == b = (M.empty, M.empty, True)
compare (TD a l1) (TD b l2)|a == b = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check.compare l1 l2
compare (TT l1) (TT l2) = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check.compare l1 l2
compare (TU n) b = (M.singleton n b, M.empty, True)
compare a (TU n) = (M.empty, M.singleton n a, True)
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
	trace ("check0: "++show o) $
	check o Top.get_types

res = Check.compare (TD "list" [TT [T "num",T "num"]]) (TD "list" [TT [T "num",T "num"]])



