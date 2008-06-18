
module Check (T (..), P (..), check_all, res) where

import Data.Map as M hiding (map, filter)

import Code hiding (res)

data P = P ([T], T) | N [Char]

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
	("to_string", TT [T "num", T "string"]):
	[]

is_val (CVal n) = True
is_val o = False
val_name (CVal n) = n

check (CNum n) et = P ([], T "num")
check (CBool n) et = P ([], T "bool")
check (CStr n) et = P ([], T "str")
check (CVal n) et =
	case M.lookup n et of
		Just a -> P ([], a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K p)) et =
	case check a et of
		P ([], TT p1) ->
			ch p1 p et []
			where
				ch p1 p2 et u =
					case (p1, p2) of
						((p1:p1s), (p2:p2s)) ->
							case check p2 et of
								P (u1, r) ->
									case merge (setu p1 u) (setu r u) of
										(u2, True) ->
											ch p1s p2s et ((u1++u2++u))
										(_, False) ->
											N $ "expected "++(show $ setu p1 u)++" actual "++(show $ setu r u)
								o -> o
						(r:[], []) -> P (u, setu r u)
--						(r:[], []) -> error $ show u
						(r, []) ->  P (u, setu (TT r) u)
				merge (T a) (T b)|a == b = ([], True)
				merge (TD a l1) (TD b l2)|a == b = foldr (\(u1,r1) (u2,r2) -> (u1++u2, r1 && r2)) ([], True) $ zipWith merge l1 l2
				merge TU b = ([b], True)
				merge a TU = ([a], True)
				merge t1 t2 = ([], False)
		P (_, _) -> N "err1"
		o -> o

check (CL a (S p)) et =
	case check a (putp p (take (length p) $ repeat TU) et) of
		P (us, ts) -> P ([], TT $ (map (\t -> t) us)++[ts])
		o -> o

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et

setu (TD n tt) u = TD n (map (\t -> setu t u) tt)
setu (TT tt) u = TT (map (\t -> setu t u) tt)
setu TU (t2:t2s) = t2
setu o (t2:t2s) = o
setu o [] = o

eq (TD n tt) (TD n2 tt2) = (&&) (n==n2) $ foldr (&&) True $ zipWith eq tt tt2
eq (TT tt) (TT tt2) = foldr (&&) True $ zipWith eq tt tt2
eq TU o = True
eq o o2 = o == o2

check_all o =
	check o Check.base

res = "1"



