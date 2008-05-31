
module Check (T (..), P (..), check_all, res) where

import Data.Map as M hiding (map, filter)

import Code hiding (res)

data P = P ([([Char], T)], T) | N [Char]

data T =
	T [Char]
	| TT [T]
	| TU
	deriving (Eq, Show)

base = M.fromList $
	("sum", TT [T "num", T "num", T "num"]):
	("list", TT [T "num", T "list"]):
	("joina", TT [T "num", T "list", T "list"]):
	("elist", T "list"):
	("length", TT [T "list", T "num"]):
	("to_string", TT [T "num", T "string"]):
	[]

is_val (CVal n) = True
is_val o = False
get_val_name (CVal n) = n

check (CNum n) e et = P ([], T "num")
check (CBool n) e et = P ([], T "bool")
check (CStr n) e et = P ([], T "str")
check (CVal n) e et =
	case M.lookup n et of
		Just a -> P ([], a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K p)) e et =
	case check a e et of
		P (_, TT p1) ->
			ch p1 p e et
			where
				ch p1 p2 e et =
					case (p1, p2) of
						((p1:p1s), (p2:p2s)) ->
							case (check p2 e et, ch p1s p2s e et) of
								(P (u, r), P (u2, r2))| p1 == r ->
									P $ (,) (u ++ u2) $ r2
								(P (u, r), P (u2, r2))| is_val p2 && TU == r ->
									P $ (,) ((get_val_name p2, p1):u2) $ r2
								(P (u, r), o2) ->
									N $ "expected "++(show p1)++" actual "++(show r)
								(o, o2) -> o
						(r:[], []) -> P ([], r)
						(r, []) ->  P ([], TT r)
		P (_, _) -> N "err1"
		o -> o

check (CL a (S p)) e et =
	case check a e (putp p (take (length p) $ repeat TU) et) of
		P (us, ts) -> P ([], TT $ (map (\(n, t) -> t) us)++[ts])
		o -> o

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et

check_all o =
	check o Code.base Check.base

res = "1"



