
module Check (T (..), P (..), check_all, res) where

import Data.Map as M hiding (map, filter)

import Code hiding (res)

data P = P Bool | N

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

check (CNum n) e et = ([], T "num")
check (CBool n) e et = ([], T "bool")
check (CStr n) e et = ([], T "str")
check (CVal n) e et =
	case M.lookup n et of
		Just a -> ([], a)
		Nothing -> error $ (++) "check cannot find " $ show n

check (CL a (K p)) e et =
	case snd $ check a e et of
		TT p1 ->
			ch p1 p e et
			where
				ch p1 p2 e et =
					case (p1, p2) of
						((p1:p1s), (p2:p2s))| (==) p1 $ snd $ check p2 e et ->
							(,) ((fst $ check p2 e et)++(fst $ ch p1s p2s e et)) $ snd $ ch p1s p2s e et
						((p1:p1s), (p2@(CVal n):p2s))| (==) TU $ snd $ check p2 e et ->
							(,) ((n, p1):(fst $ ch p1s p2s e et)) $ snd $ ch p1s p2s e et
						((p1:p1s), (p2:p2s)) -> (,) [] $ T $ "err2: "++(show p1)++" "++(show $ snd $ check p2 e et)
						(r:[], []) -> ([], r)
						(r, []) ->  ([], TT r)
		_ -> error "err1"

check (CL a (S p)) e et =
	([], TT $ (map (\(n, t) -> t) us)++[ts])
	where
		(us, ts) = check a e (putp p (take (length p) $ repeat TU) et)

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et

check_all o =
	check o Code.base Check.base

res = "1"



