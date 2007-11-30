module Compiler (res) where

import Parser hiding (res)
import Code hiding (res)

data P = P C | N
	deriving (Eq, Show)

compile s =
	P (CNum 2)

tests = [
	(Sn 2, CNum 1)
	,(Sn 2, CNum 1)
	,(Sn 12, CNum 1)
	,(Ss "sum", CNum 1)
	,(Scall (Ss "sum") (SynK [Ss "one"]), CNum 1)
	,(Scall (Ss "sum") (SynK [Sn 11, Sn 22]), CNum 1)
	,(Scall (Ss "sum") (SynK [Sn 11, Scall (Ss "min") (SynK [Sn 22, Sn 33])]), CNum 1)
--	,(Scall (Ss "incr") (SynK [Scall (Ss "min") (SynK [Sn 22, Sn 33])]))
--	,(Scall (Ss "incr") (SynK [Scall (Ss "min") (SynK [Sn 22, Sn 33])]))
--	,(Scall (Scall (Ss "sum") (SynK [Sn 1])) (SynS ["a", "b"]))
--	,(Scall (Scall (Scall (Scall (Ss "sum") (SynK [Sn 1,Scall (Ss "min") (SynK [Sn 22,Ss "z"])])) (SynS ["a","b"])) (SynK [Scall (Ss "min") (SynK [Ss "z"])])) (SynS ["x","y"]))
--	,(Scall (Scall (Scall (Ss "sum") (SynK [Ss "a", Ss "b"])) (SynS ["a", "b"])) (SynK [Sn 12, Sn 22]))
--	,((Scall (Scall (Scall (Ss "if") (SynK [Scall (Ss "less") (SynK [Ss "_",Sn 5]),Scall (Ss "sum") (SynK [Ss "_",Scall (Ss "_r") (SynK [Scall (Ss "sum") (SynK [Ss "_",Sn 1])])]),Ss "_"])) (SynS ["_"]))) (SynM [MarkR]))
--	,((Scall (Scall (Scall (Ss "_") (SynK [Scall (Ss "list") (SynK [Sn 1,Sn 2,Sn 3,Sn 4,Sn 5])])) (SynS ["_"])) (SynK [Scall (Scall (Scall (Ss "if") (SynK [Scall (Ss "is_empty") (SynK [Ss "_"]),Ss "list",Scall (Scall (Ss "join") (SynK [Scall (Ss "_r") (SynK [Scall (Ss "filter") (SynK [Scall (Ss "le") (SynK [Ss "h"]),Ss "_"])]),Ss "h",Scall (Ss "join") (SynK [Scall (Ss "list") (SynK [Ss "h"]),Scall (Ss "_r") (SynK [Scall (Ss "filter") (SynK [Scall (Ss "more") (SynK [Ss "h"]),Ss "_"])])])])) (SynS ["h","t"]),Scall (Ss "head") (SynK [Ss "_"]),Scall (Ss "tail") (SynK [Ss "_"])])) (SynS ["_"])) (SynM [MarkR])])))
	]

mk_test (s, e) =
	(case compile s of
		P s2|e == s2 -> "ok - "
		P s2 -> "ce"++"("++(show s2)++") - "
		N -> "ce - ") ++ show s

res = foldr1 (\a b -> a++"\n"++b) $ map mk_test tests


