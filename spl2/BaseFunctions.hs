module BaseFunctions where

import Data.Map as M

import Types

data Fun = Fun C T

base = M.fromList $
	("sum", Fun
		(CL (CInFun 2 (InFun "sum" do_sum)) (K []))
		(TT [T "num", T "num", T "num"]))
	:("elist", Fun
		(CList [])
		(TD "list" [TU "a"]))
	:("head", Fun
		(CNum 1)
		(TT [TD "list" [TU "a"], TU "a"]))
	:("joina", Fun
		(CL (CInFun 2 (InFun "joina" do_joina)) (K []))
		(TT [TU "a", TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("length", Fun
		(CL (CInFun 1 (InFun "length" do_length)) (K []))
		(TT [TD "list" [TU "a"], T "num"]))
	:("to_string", Fun
		(CL (CInFun 1 (InFun "to_string" do_to_string)) (K []))
		(TT [TU "a", T "string"]))
	:("debug", Fun
		(CNum 1)
		(TT [TU "a", TU "a"]))
	:[]

get_code (Fun c _) = c
get_type (Fun _ t) = t

get_codes = M.map get_code base
get_types = M.map get_type base

-- native functions
do_sum (CNum a:CNum b:[]) e = CNum (a+b)
do_sum o e = error ("do_sum"++show o)

do_joina (a:CList b:[]) e =	CList (a:b)
do_joina o e = error $ show o

do_length (CList l:[]) e = CNum (length l)

do_to_string (a:[]) e = CStr (show a)


