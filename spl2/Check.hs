
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
		Just (TT (h:t)) -> 
			case foldr (&&) True $ zipWith (\a b -> b == check a e et) p t of
				True -> T "super"
				False -> T "super2"
		Just (T v) -> T "super3"
		Nothing -> error $ "cannot find "++show n
check (CL (CInFun n i f) (K p)) e et|i > length p =
	T "goto"
check o e et = error $ "type error: "++show o

check_all o =
	check o Code.base Check.base

res = "1"

