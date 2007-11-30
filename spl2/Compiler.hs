module Compiler (res) where

import Parser hiding (res)
import Code hiding (res)

data P = P C | N
	deriving (Eq, Show)

comp (Sn x) =
	CNum x

comp (Ss s) =
	CVal s

comp (Scall f (SynK a)) =
	CL (comp f) (K (map comp a))

comp (Scall f (SynS a)) =
	CL (comp f) (S a)

--comp (Scall f (SynM a)) =
--	CL (comp f) (S a)

compile s =
	P (comp s)

tests = [
	(Sn 2, CNum 2)
	,(Sn 12, CNum 12)
	,(Ss "sum", CVal "sum")
	,(Scall (Ss "sum") (SynK [Ss "one"]), CL (CVal "sum") (K [CVal "one"]))
	,(Scall (Ss "sum") (SynK [Sn 11, Sn 22]), CL (CVal "sum") (K [CNum 11,CNum 22]))
	,(Scall (Ss "sum") (SynK [Sn 11, Scall (Ss "min") (SynK [Sn 22, Sn 33])]), CL (CVal "sum") (K [CNum 11,CL (CVal "min") (K [CNum 22,CNum 33])]))
	,(Scall (Ss "incr") (SynK [Scall (Ss "min") (SynK [Sn 22, Sn 33])]), CL (CVal "incr") (K [CL (CVal "min") (K [CNum 22,CNum 33])]))
	,(Scall (Scall (Ss "sum") (SynK [Sn 1])) (SynS ["a", "b"]), CNum 1)
	,(Scall (Scall (Scall (Scall (Ss "sum") (SynK [Sn 1,Scall (Ss "min") (SynK [Sn 22,Ss "z"])])) (SynS ["a","b"])) (SynK [Scall (Ss "min") (SynK [Ss "z"])])) (SynS ["x","y"]), CNum 1)
	,(Scall (Scall (Scall (Ss "sum") (SynK [Ss "a", Ss "b"])) (SynS ["a", "b"])) (SynK [Sn 12, Sn 22]), CNum 1)
--	,((Scall (Scall (Scall (Ss "if") (SynK [Scall (Ss "less") (SynK [Ss "_",Sn 5]),Scall (Ss "sum") (SynK [Ss "_",Scall (Ss "_r") (SynK [Scall (Ss "sum") (SynK [Ss "_",Sn 1])])]),Ss "_"])) (SynS ["_"]))) (SynM [MarkR]), CNum 1)
--	,((Scall (Scall (Scall (Ss "_") (SynK [Scall (Ss "list") (SynK [Sn 1,Sn 2,Sn 3,Sn 4,Sn 5])])) (SynS ["_"])) (SynK [Scall (Scall (Scall (Ss "if") (SynK [Scall (Ss "is_empty") (SynK [Ss "_"]),Ss "list",Scall (Scall (Ss "join") (SynK [Scall (Ss "_r") (SynK [Scall (Ss "filter") (SynK [Scall (Ss "le") (SynK [Ss "h"]),Ss "_"])]),Ss "h",Scall (Ss "join") (SynK [Scall (Ss "list") (SynK [Ss "h"]),Scall (Ss "_r") (SynK [Scall (Ss "filter") (SynK [Scall (Ss "more") (SynK [Ss "h"]),Ss "_"])])])])) (SynS ["h","t"]),Scall (Ss "head") (SynK [Ss "_"]),Scall (Ss "tail") (SynK [Ss "_"])])) (SynS ["_"])) (SynM [MarkR])])))
	]

mk_test (s, e) =
	(case compile s of
		P s2|e == s2 -> "ok - "
		P s2 -> "ce"++"("++(show s2)++") - "
		N -> "ce - ") ++ show s

res = foldr1 (\a b -> a++"\n"++b) $ map mk_test tests


