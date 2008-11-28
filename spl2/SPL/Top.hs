module SPL.Top where

import Data.Map as M

import SPL.Types
import SPL.Code

data Fun = Fun C T

eval0 c =
	eval c get_codes

base = M.fromList $
	("sum", Fun
		(CL (CInFun 2 (InFun "" do_sum) 0) (K []) 0)
		(TT [T "num", T "num", T "num"]))
	:("eq", Fun
		(CL (CInFun 2 (InFun "" do_eq) 0) (K []) 0)
		(TT [T "num", T "num", T "boolean"]))
	:("less", Fun
		(CL (CInFun 2 (InFun "" do_less) 0) (K []) 0)
		(TT [T "num", T "num", T "boolean"]))
	:("not", Fun
		(CL (CInFun 1 (InFun "" do_not) 0) (K []) 0)
		(TT [T "boolean", T "boolean"]))
	:("incr", Fun
		(CL (CInFun 1 (InFun "" do_incr) 0) (K []) 0)
		(TT [T "num", T "num"]))
	:("elist", Fun
		(CList [] 0)
		(TD "list" [TU "a"]))
	:("head", Fun
		(CL (CInFun 1 (InFun "" do_head) 0) (K []) 0)
		(TT [TD "list" [TU "a"], TU "a"]))
	:("tail", Fun
		(CL (CInFun 1 (InFun "" do_tail) 0) (K []) 0)
		(TT [TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("filter", Fun
		(CL (CInFun 2 (InFun "" do_filter) 0) (K []) 0)
		(TT [TT [TU "a", T "boolean"], TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("join1", Fun
		(CL (CInFun 2 (InFun "" do_join1) 0) (K []) 0)
		(TT [TU "a", TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("concat", Fun
		(CL (CInFun 2 (InFun "" do_concat) 0) (K []) 0)
		(TT [TD "list" [TU "a"], TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("length", Fun
		(CL (CInFun 1 (InFun "" do_length) 0) (K []) 0)
		(TT [TD "list" [TU "a"], T "num"]))
	:("to_string", Fun
		(CL (CInFun 1 (InFun "" do_to_string) 0) (K []) 0)
		(TT [TU "a", T "string"]))
	:("pair", Fun
		(CL (CInFun 2 (InFun "" do_pair) 0) (K []) 0)
		(TT [TU "a", TU "b", TD "pair" [TU "a",TU "b"]]))
	:("debug", Fun
		(CL (CInFun 1 (InFun "" do_debug) 0) (K []) 0)
		(TT [TU "a", TU "a"]))
	:("go", Fun
		(CNum 0 0)
		TL)
	:("iff", Fun
		(CL (CInFun 2 (InFun "" do_iff) 0) (K []) 0)
		(TT [TD "list" [TD "pair" [T "boolean", TT [TL, TU "a"]]], TT [TL, TU "a"], TU "a"]))
	:[]

put_name n (CL (CInFun i (InFun "" f) ii) (K []) iii) = CL (CInFun i (InFun n f) ii) (K []) iii
put_name n o = o

get_code n (Fun c _) = put_name n c
get_type (Fun _ t) = t

get_codes = M.mapWithKey get_code base
get_types = M.map get_type base

-- native functions
do_sum (CNum a _:CNum b _:[]) e = CNum (a+b) 0
do_sum o e = error ("do_sum"++show o)

do_less (CNum a _:CNum b _:[]) e = CBool (a < b) 0
do_eq (CNum a _:CNum b _:[]) e = CBool (a == b) 0
do_not (CBool b _:[]) e = CBool (not b) 0

do_incr (CNum a _:[]) e = CL (CVal "sum" 0) (K [CNum a 0, CNum 1 0]) 0
do_incr o e = error ("do_incr"++show o)

do_join1 (a:CList b _:[]) e = CList (a:b) 0
do_join1 o e = error $ show o
do_concat (CList a _:CList b _:[]) e = CList (a++b) 0

do_head (CList a _:[]) e = head a
do_tail (CList a _:[]) e = CList (tail a) 0
cbool_val (CBool b _) = b
do_filter (a:CList l _:[]) e = CList (Prelude.filter (\x -> cbool_val $ eval (CL a (K [x]) 0) e) l) 0

do_length (CList l _:[]) e = CNum (length l) 0

do_debug (a:[]) e = a

do_to_string (a:[]) e = CStr (show a) 0

do_pair (a:b:[]) e = CPair (a:b:[]) 0

do_iff (CList [] _:CL c L _:[]) e = CL (CL c L 0) (K [CVal "go" 0]) 0
do_iff (CList ((CPair (CBool True _:exp:[]) _):is) _:CL c L _:[]) e = CL exp (K [CVal "go" 0]) 0
do_iff (CList ((CPair (CBool False _:exp:[]) _):is) _:CL c L _:[]) e = do_iff (CList is 0:CL c L 0:[]) e
do_iff o e = error ("do_iff: "++show o)


