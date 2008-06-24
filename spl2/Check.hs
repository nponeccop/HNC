
module Check (T (..), P (..), check_all, res) where

import Data.Map as M hiding (map, filter)

import Code hiding (res)

data P = P ([T], [T], T) | N [Char]

data T =
	T [Char]
	| TT [T]
	| TU
	| TD [Char] [T]
	deriving (Eq, Show)

base = M.fromList $
	("sum", TT [T "num", T "num", T "num"]):
	("list", TT [T "num", T "list"]):
	("joina", TT [TU, TD "list" [TU], TD "list" [TU]]):
	("elist", TD "list" [TU]):
	("head", TT [TD "list" [TU], TU]):
	("length", TT [TD "list" [TU], T "num"]):
	("to_string", TT [TU, T "string"]):
	("debug", TT [TU, TU]):
	[]

is_val (CVal n) = True
is_val o = False
val_name (CVal n) = n

check (CNum n) et = P ([], [], T "num")
check (CBool n) et = P ([], [], T "bool")
check (CStr n) et = P ([], [], T "str")
check (CVal n) et =
	case M.lookup n et of
		Just a -> P ([], [], a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K p)) et =
	case check a et of
		P ([], [], TT p1) ->
			ch p1 p et [] []
			where
				ch p1 p2 et ul ur =
					case (p1, p2) of
						((p1:p1s), (p2:p2s)) ->
							case check p2 et of
								P (u1l, u1r, r) ->
									case compare (setu p1 ul) (setu r ul) of
										(u2l, u2r, True) ->
											ch p1s p2s et ((u1l++u2l++ul)) urr
											where
												urr =
													case uurr of
														[] -> u2r
														l -> map (\a -> setu a u2r) l
													where
														uurr =
															case ur of -- change to merge
																[] -> u1r
																l -> l
										(_, _, False) ->
											N $ "expected "++(show $ setu p1 ul)++" actual "++(show $ setu r ul)
								o -> o
						(r:[], []) -> P (ul, ur, setu r ul)
--						(r:[], []) -> error $ show u
						(r, []) ->  P (ul, ur, setu (TT r) ul)
				compare (T a) (T b)|a == b = ([], [], True)
				compare (TD a l1) (TD b l2)|a == b = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (u1l++u2l, u1r++u2r, r1 && r2)) ([], [], True) $ zipWith compare l1 l2
				compare TU b = ([b], [], True)
				compare a TU = ([], [a], True)
				compare t1 t2 = ([], [], False)
		P (_, _, _) -> N "err1"
		o -> o

check (CL a (S p)) et =
	case check a (putp p (take (length p) $ repeat TU) et) of
		P (ul, ur, ts) -> P ([], [], TT $ (map (\t -> t) ur)++[ts])
		o -> o

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et

setu (TD n tt) u = TD n (map (\t -> setu t u) tt)
setu (TT tt) u = TT (map (\t -> setu t u) tt)
setu TU (t2:t2s) = t2
setu o (t2:t2s) = o
setu o [] = o

check_all o =
	check o Check.base

res = "1"



