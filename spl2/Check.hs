
module Check (P (..), check0, res) where

import Data.Map as M hiding (filter)

import Types
import Code hiding (res)
import Top

data P = P (Map [Char] T, T) | N [Char]
	deriving Show


check::C -> Map [Char] T -> P
check (CNum n) et = P (M.empty, T "num")
check (CBool n) et = P (M.empty, T "boolean")
check (CStr n) et = P (M.empty, T "string")
check (CVal n) et =
	case M.lookup n et of
		Just a -> P (M.empty, a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K p)) et =
	case (f, ps_err) of
		(P (rm, TT r), [])|M.null rm ->
			ch r (setml ps_ok rm) et ps_rm ps_rm -- 2nd union ?
			where
				ch (p1:p1s) (p2:p2s) et ul ur =
					case Check.compare (setm p1 ul) (setm p2 ul) of
						(u2l, u2r, True) ->
							ch p1s p2s et ull urr
							where
								ull = M.union (merge ul u2l) u2l --M.union ul u2l
								merge a b = M.intersection a b
								urr = case M.null ur of
									True -> u2r;
									False -> M.map (\a -> setm a u2r) ur
						(_, _, False) ->
							N $ "expected "++(show $ setm p1 ul)++", actual "++(show $ setm p2 ul)
				ch (p1:[]) [] et ul ur = P (ur, setm p1 ul)
				ch ([]) [] et ul ur = N ("too many parameters for "++(show a))
				ch (p1) [] et ul ur = P (ur, setm (TT p1) ul)
		(P (rm, TU n), [])|M.null rm ->
			P (M.singleton n (TT (ps_ok++[TU ('_':n)])), TU ('_':n))
		(P _, (o:os)) ->
			o
		(o, o2) -> o
	where
		f =
			case (a, ps_err) of
				(CL _ (S p), []) -> check_s a ps_ok et
				(a, []) -> check a et
				(_, (o:os)) -> o
		ps = Prelude.map (\a -> check a et) p
		ps_err = filter (\a -> case a of N _ -> True; P _ -> False) ps
		ps_ok2 = filter (\a -> case a of P _ -> True; N _ -> False) ps
		ps_rm = M.unions $ Prelude.map (\a -> case a of P (a, b) -> a; N _ -> error "err100") ps_ok2
		ps_ok = Prelude.map (\a -> case a of P (a, b) -> b; N _ -> error "err100") ps_ok2

check (CL a@(CL a2 (S (p1:p1s))) (K (p2:p2s))) et =
	case check p2 et of 
		P (_, b) -> check a (putp [p1] [b] et)
		o -> o

check (CL a L) et =
	case check a et of
		P (ur, r) ->
			P (ur, TT [TL, r])
		o -> o

check (CL a R) et =
	case check a (putp ["_f"] [TU "_f"] et) of
		P (ur, r) -> check a (putp ["_f"] [r] et)
		o -> o

check a@(CL f (S p)) et =
	check_s a tus et
	where
		tus = Prelude.map (TU) p

check_s (CL a (S p)) pp et =
	case check a et2 of
		P (ur, ts) ->
			P (M.empty , TT $ ( -- is it ok to use p as name of unknown type ?
				Prelude.map (\n ->
					case M.lookup n ur of
						Just t -> t
						Nothing -> TU n) p
			)++[ts])
		o -> o
	where
		et2 = putp p (take (length p) $ zipWith (\a b ->
			case b of
				TU _ -> TU a
				o -> o
			) p pp) et

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

setml l u = Prelude.map (\a -> setm a u) l
setm (TD n tt) u = TD n (Prelude.map (\t -> setm t u) tt)
setm (TT tt) u = TT (Prelude.map (\t -> setm t u) tt)
setm (TU n) u =
	case M.lookup n u of
		Just a -> a
		Nothing -> TU n
setm o u = o

check0 o =
	check o Top.get_types

res = Check.compare (TD "list" [TT [T "num",T "num"]]) (TD "list" [TT [T "num",T "num"]])



