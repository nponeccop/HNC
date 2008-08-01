module Top where

import Data.Map as M

import Types
import Code

data Fun = Fun C T

eval0 c =
	eval c get_codes

base = M.fromList $
	("sum", Fun
		(CL (CInFun 2 (InFun "" do_sum)) (K []))
		(TT [T "num", T "num", T "num"]))
	:("less", Fun
		(CL (CInFun 2 (InFun "" do_less)) (K []))
		(TT [T "num", T "num", T "boolean"]))
	:("not", Fun
		(CL (CInFun 1 (InFun "" do_not)) (K []))
		(T "boolean"))
	:("incr", Fun
		(CL (CInFun 1 (InFun "" do_incr)) (K []))
		(TT [T "num", T "num"]))
	:("elist", Fun
		(CList [])
		(TD "list" [TU "a"]))
	:("head", Fun
		(CL (CInFun 1 (InFun "" do_head)) (K []))
		(TT [TD "list" [TU "a"], TU "a"]))
	:("tail", Fun
		(CL (CInFun 1 (InFun "" do_tail)) (K []))
		(TT [TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("filter", Fun
		(CL (CInFun 2 (InFun "" do_filter)) (K []))
		(TT [TT [TU "a", T "boolean"], TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("join1", Fun
		(CL (CInFun 2 (InFun "" do_join1)) (K []))
		(TT [TU "a", TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("concat", Fun
		(CL (CInFun 2 (InFun "" do_concat)) (K []))
		(TT [TD "list" [TU "a"], TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("length", Fun
		(CL (CInFun 1 (InFun "" do_length)) (K []))
		(TT [TD "list" [TU "a"], T "num"]))
	:("to_string", Fun
		(CL (CInFun 1 (InFun "" do_to_string)) (K []))
		(TT [TU "a", T "string"]))
	:("pair", Fun
		(CL (CInFun 2 (InFun "" do_pair)) (K []))
		(TT [TU "a", TU "b", TD "pair" [TU "a",TU "w"]]))
	:("debug", Fun
		(CL (CInFun 1 (InFun "" do_debug)) (K []))
		(TT [TU "a", TU "a"]))
	:("go", Fun
		(CNum 0)
		TL)
	:("iff", Fun
		(CL (CInFun 2 (InFun "" do_iff)) (K []))
		(TT [TD "list" [TD "pair" [T "boolean", TT [TL, TU "a"]]], TT [TL, TU "a"], TU "a"]))
	:[]

put_name n (CL (CInFun i (InFun "" f)) (K [])) = CL (CInFun i (InFun n f)) (K [])
put_name n o = o

get_code n (Fun c _) = put_name n c
get_type (Fun _ t) = t

get_codes = M.mapWithKey get_code base
get_types = M.map get_type base

-- native functions
do_sum (CNum a:CNum b:[]) e = CNum (a+b)
do_sum o e = error ("do_sum"++show o)

do_less (CNum a:CNum b:[]) e = CBool (a < b)
do_not (CBool b:[]) e = CBool (not b)

do_incr (CNum a:[]) e = CL (CVal "sum") (K [CNum a, CNum 1])
do_incr o e = error ("do_incr"++show o)

do_join1 (a:CList b:[]) e = CList (a:b)
do_join1 o e = error $ show o
do_concat (CList a:CList b:[]) e = CList (a++b)

do_head (CList a:[]) e = head a
do_tail (CList a:[]) e = CList (tail a)
cbool_val (CBool b) = b
do_filter (a:CList l:[]) e = CList $ Prelude.filter (\x -> cbool_val $ eval (CL a (K [x])) e) l

do_length (CList l:[]) e = CNum (length l)

do_debug (a:[]) e = a

do_to_string (a:[]) e = CStr (show a)

do_pair (a:b:[]) e = CPair (a:b:[])

do_iff (CList []:CL c L:[]) e = CL (CL c L) (K [CVal "go"])
do_iff (CList ((CPair (CBool True:exp:[])):is):CL c L:[]) e = CL exp (K [CVal "go"])
do_iff (CList ((CPair (CBool False:exp:[])):is):CL c L:[]) e = do_iff (CList is:CL c L:[]) e
do_iff o e = error ("do_iff: "++show o)


