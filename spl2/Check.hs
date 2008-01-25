
module Check (P (..), check_all, res) where

import Data.Map as M

import Code hiding (res)

data P = P Bool | N

--base = M.fromList $
--	("sum", ):
--	[]

check (CNum n) e = True
check (CBool n) e = True
check (CVal v) e =
	case M.lookup v e of
		Just v -> check v e
		Nothing -> error ("cannot find "++show v)
check (CStr n) e = True
check (CL n p) e = check n e
check o tb = error $ (++) "type error: " $ (show o)

check_all o =
	check o Code.base

res = "1"

