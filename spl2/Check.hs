
module Check (P (..), check_all, res) where

import Data.Map as M hiding (filter)

import Types
import Code hiding (res)
import BaseFunctions

data P = P (Map [Char] T, T) | N [Char]

{-base = M.fromList $
	("sum", TT [T "num", T "num", T "num"]):
	("list", TT [T "num", T "list"]):
	("pair", TT [TU "a", TU "b", TD "pair" [TU "a", TU "b"]]):
	("joina", TT [TU "a", TD "list" [TU "a"], TD "list" [TU "a"]]):
	("elist", TD "list" [TU "a"]):
	("head", TT [TD "list" [TU "a"], TU "a"]):
	("length", TT [TD "list" [TU "a"], T "num"]):
	("to_string", TT [TU "a", T "string"]):
	("debug", TT [TU "a", TU "a"]):
	[]-}

is_val (CVal n) = True
is_val o = False
val_name (CVal n) = n

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
		P (rm, p1)|M.null rm ->
			ch p1 p et M.empty M.empty
			where
				ch p1 p2 et ul ur =
					case (p1, p2) of
						(TT (p1:p1s), (p2:p2s)) ->
							case check p2 et of
								P (u1r, r) ->
									case Check.compare (setm p1 ul) (setm r ul) of
										(u2l, u2r, True) ->
											ch (TT p1s) p2s et ull urr
											where
												ull = M.unions [ul, u2l]
												urr = case M.null uurr of True -> u2r; False -> M.map (\a -> setm a u2r) uurr
												uurr = M.unions [ur, u1r]
{-												merge (T a) (T b)|a == b = T a
												merge (TD a l1) (TD b l2)|a == b = TD a (zipWith merge l1 l2)
												merge (TU n) b = b
												merge a (TU n) = a
												merge t1 t2 = error ("merge error: "++show t1++", "++show t2)
-}
										(_, _, False) ->
											N $ "expected "++(show $ setm p1 ul)++", actual "++(show $ setm r ul)
								o -> o
						(TT (r:[]), []) -> P (ur, setm r ul)
						(TT r, []) ->  P (ur, setm (TT r) ul)
						(p1, (p2:p2s)) ->
							case check p2 et of
								P (u1r, r) ->
									case Check.compare (setm p1 ul) (setm r ul) of
										(u2l, u2r, True) ->
											ch p1 p2s et ull urr
											where
												ull = M.unions [ul, u2l]
												urr = case M.null uurr of True -> u2r; False -> M.map (\a -> setm a u2r) uurr
												uurr = M.unions [ur, u1r]
						(r, []) -> P (ur, r)
		P (rm, p) -> N ("err1: "++show rm++", "++show p)
		o -> o

check (CL a (S p)) et =
	case check a et2 of
		P (ur, ts) ->
			P (M.empty, TT $ ( -- is it ok to use p as name of unknown type ?
				Prelude.map (\n -> case M.lookup n ur of Just t -> t; Nothing -> TU n) p
			)++[ts])
		o -> o
	where
		et2 = putp p (take (length p) $ Prelude.map (TU) p) et

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et

compare (T a) (T b)|a == b = (M.empty, M.empty, True)
compare (TD a l1) (TD b l2)|a == b = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check.compare l1 l2
compare (TT l1) (TT l2) = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check.compare l1 l2
compare (TU n) b = (M.singleton n b, M.empty, True)
compare a (TU n) = (M.empty, M.singleton n a, True)
compare t1 t2 = (M.empty, M.empty, False)

setu (TD n tt) u = TD n (Prelude.map (\t -> setu t u) tt)
setu (TT tt) u = TT (Prelude.map (\t -> setu t u) tt)
setu (TU n) (t2:t2s) = t2
setu o (t2:t2s) = o
setu o [] = o

setm (TD n tt) u = TD n (Prelude.map (\t -> setm t u) tt)
setm (TT tt) u = TT (Prelude.map (\t -> setm t u) tt)
setm (TU n) u =
	case M.lookup n u of
		Just a -> a
		Nothing -> TU n
setm o u = o

check_all o =
	check o BaseFunctions.get_types

res = Check.compare (TD "list" [TT [T "num",T "num"]]) (TD "list" [TT [T "num",T "num"]])



