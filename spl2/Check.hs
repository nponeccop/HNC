
module Check (T (..), P (..), check_all, res) where

import Data.Map as M hiding (map, filter)

import Code hiding (res)

data P = P ([([Char], T)], T) | N [Char]

data T =
	T [Char]
	| TT [T]
	| TU
	| TD [Char] T
	deriving (Eq, Show)

base = M.fromList $
	("sum", TT [T "num", T "num", T "num"]):
	("list", TT [T "num", T "list"]):
	("joina", TT [TU, TD "list" TU, TD "list" TU]):
	("elist", TD "list" TU):
	("length", TT [T "list", T "num"]):
	("to_string", TT [T "num", T "string"]):
	[]

is_val (CVal n) = True
is_val o = False
get_val_name (CVal n) = n

check (CNum n) e et u = P ([], T "num")
check (CBool n) e et u = P ([], T "bool")
check (CStr n) e et u = P ([], T "str")
check (CVal n) e et u =
	case M.lookup n et of
		Just a -> P ([], a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K p)) e et u =
	case check a e et u of
		P ([], TT p1) ->
			ch p1 p e et u
			where
				ch p1 p2 e et u =
					case (p1, p2) of
						((p1:p1s), (p2:p2s)) ->
							case (check p2 e et u, ch p1s p2s e et u) of
								(P (u1, r), P (u2, r2))| p1 == r ->
									P $ (,) (u1 ++ u2) $ r2
								(P (u1, r), P (u2, r2))| is_val p2 && TU == r ->
									P $ (,) ((get_val_name p2, p1):u2) $ r2
								(P (u1, r), P (u2, r2))| TU == p1 ->
									ch p1s p2s e et (r:u)
								(P (u1, r), o2) ->
									N $ "expected "++(show $ setu p1 u)++" actual "++(show r)
								(o, o2) -> o
						(r:[], []) -> P ([], setu r u)
						(r, []) ->  P ([], setu (TT r) u)
		P (_, _) -> N "err1"
		o -> o

check (CL a (S p)) e et u =
	case check a e (putp p (take (length p) $ repeat TU) et) u of
		P (us, ts) -> P ([], TT $ (map (\(n, t) -> t) us)++[ts])
		o -> o

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et

setu (TD n t) u = TD n (setu t u)
setu (TT tt) u = TT (map (\t -> setu t u) tt)
setu TU (t2:t2s) = t2
setu o (t2:t2s) = o
setu o [] = o

check_all o =
	check o Code.base Check.base []

res = "1"



