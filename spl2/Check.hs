
module Check (P (..), check, res) where

import Code hiding (res)

data P = P Bool | N

check (CNum n) = P True
check (CBool n) = P True
check (CVal n) = P True
check (CStr n) = P True
--check (CL n) = P True
check o = error (show o)

res = "1"

