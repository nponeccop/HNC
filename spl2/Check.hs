
module Check (T (..), P (..), check_all, res) where

import Data.Map as M hiding (map)

import Code hiding (res)

data P = P Bool | N

data T =
	T [Char]
	| TT [T]
	deriving (Eq, Show)

base = M.fromList $
	("sum", TT [T "num", T "num", T "num"]):
	("list", TT [T "num", T "list"]):
	("length", TT [T "list", T "num"]):
	("to_string", TT [T "num", T "string"]):
	[]

check (CNum n) e et = T "num"
check (CBool n) e et = T "bool"
check (CStr n) e et = T "str"
check (CVal v) e et =
	case M.lookup v e of
		Just v -> check v e et
		Nothing -> error $ "cannot find "++show v
check (CL (CInFun n i f) (K p)) e et|i == length p =
	case M.lookup n et of
		Just (TT l) -> ch l p
			where
				ch (x:[]) [] = x
				ch (x:xs) (x2:xs2) =
					case x == (check x2 e et) of
						True -> ch xs xs2
						False -> T ("super2b:" ++ (show $ check x2 e et)++"|"++show x)
				ch e e2 = error "err2"
		Just (T v) -> T "super2"
		Nothing -> error $ "cannot find "++show n
check (CL (CInFun n i f) (K p)) e et|i > length p =
	case M.lookup n et of
		Just (TT l) -> ch l p
			where
				ch l z = TT l
				ch (x:xs) (x2:xs2) =
					case x == (check x2 e et) of
						True -> ch xs xs2
						False -> T "super7b"
		Just (T v) -> T "super 7"
		Nothing -> T "super 8"
check (CL (CInFun n i f) (K p)) e et|i < length p =
	error "type too many values"
check (CL (CInfFun n f) (K p)) e et =
	case M.lookup n et of
		Just (TT (h:r:[])) -> ch p
			where
				ch [] = r
				ch (p:ps) =
					case h == (check p e et) of
						True -> ch ps
						False -> T "super 9b"
				ch e = error "err9"
		Just o -> error "err9b"
		Nothing -> T "super 9"
check (CL a@(CVal v) (K p)) e et =
	case M.lookup v e of
		Just v -> check (CL v (K p)) e et
		Nothing -> error $ "cannot find "++show v
check (CL (CL c (K p1)) (K p2)) e et = check (CL c (K (p1++p2))) e et
check o e et = error $ "type error: "++show o

check_all o =
	check o Code.base Check.base

res = "1"

