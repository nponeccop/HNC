module SPL.Top where

import Data.Map as M

import System.IO.Unsafe
import SPL.Parser
import SPL.Compiler
import SPL.Types
import SPL.Code
import SPL.Check3
import qualified SPL.Lib.Str
import Debug.Trace

data Fun = Fun C T | Lib [Char]

observe s v = trace ("{"++s++":"++show v++"}") v
observeN s v = v

check0 o = check_with_rename o SPL.Top.get_types False
check1 o e = check_with_rename o e True
check2 o = check_with_rename o SPL.Top.get_types True

eval0 c =
	eval c get_codes

base = M.fromList $
	("sum", Fun
		(CL (CInFun 2 (InFun "" do_sum)) (K []))
		(TT [T "num", T "num", T "num"]))
	:("eq", Fun
		(CL (CInFun 2 (InFun "" do_eq)) (K []))
		(TT [T "num", T "num", T "boolean"]))
	:("less", Fun
		(CL (CInFun 2 (InFun "" do_less)) (K []))
		(TT [T "num", T "num", T "boolean"]))
	:("not", Fun
		(CL (CInFun 1 (InFun "" do_not)) (K []))
		(TT [T "boolean", T "boolean"]))
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
	:("reverse", Fun
		(CL (CInFun 1 (InFun "" do_reverse)) (K []))
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
		(TT [TU "a", TU "b", TD "pair" [TU "a",TU "b"]]))
	:("debug", Fun
		(CL (CInFun 1 (InFun "" do_debug)) (K []))
		(TT [TU "a", TU "a"]))
	:("go", Fun
		(CNum 0)
		TL)
	:("iff", Fun
		(CL (CInFun 2 (InFun "" do_iff)) (K []))
		(TT [TD "list" [TD "pair" [T "boolean", TT [TL, TU "a"]]], TT [TL, TU "a"], TU "a"]))
	:("if", Fun
		(CL (CInFun 3 (InFun "" do_if)) (K []))
		(TT [T "boolean",TT [TL, TU "a"], TT [TL, TU "a"], TU "a"]))
	:("foldr", Fun
		(CL (CInFun 3 (InFun "" do_foldr)) (K []))
		(TT [TT [TU "a", TU "b", TU "b"], TU "b", TD "list" [TU "a"], TU "b"]))
	:("print", Fun
		(CNum 0)
		(TT [TU "a", TD "IO" [T "void"]] ))
	:("bind", Fun
		(CNum 0)
		(TT [ TD "IO" [TU "t1"], TT [TU "t1", TD "IO" [TU "t2"]], TD "IO" [TU "t2"]]))
	:("readnum", Fun
		(CNum 0)
		(TD "IO" [T "num"] ))
	:("voidbind", Fun
		(CNum 0)
		(TT [ TD "IO" [T "void"], TD "IO" [TU "a"], TD "IO" [TU "a"]]))
	:("natrec", Fun
		(CNum 0)
		(TT [TT [T "num", TU "a", TU "a"], TU "a", T "num", TU "a"]))			 	
	:("load", Fun
		(CL (CInFun 1 (InFun "" do_load)) (K []))
		(TT [T "string", TU "a"]))
	:("out", Fun
		(CL (CInFun 1 (InFun "" do_out)) (K []))
		(TT [T "string", T "void"]))
	:("str", Fun
		(CStruct $ M.fromList
			[("concat", CL (CInFun 2 (InFun "" do_str_concat)) (K []))])
		(TS $ M.fromList
			[("concat", TT [T "string", T "string", T "string"])]
			))
	:("to", Fun
		(CL (CInFun 1 (InFun "" do_to)) (K []))
		(TT [T "num", TD "list" [T "num"]]))
	:("mod", Fun
		(CL (CInFun 2 (InFun "" do_mod)) (K []))
		(TT [T "num", T "num", T "num"]))
	:("div", Fun
		(CL (CInFun 2 (InFun "" do_div)) (K []))
		(TT [T "num", T "num", T "num"]))
	:("or", Fun
		(CL (CInFun 2 (InFun "" do_or)) (K []))
		(TT [T "boolean", T "boolean", T "boolean"]))
	:("map", Fun
		(CL (CInFun 2 (InFun "" do_map)) (K []))
		(TT [TT [TU "a", TU "a"], TD "list" [TU "a"], TD "list" [TU "a"]]))
	:("str", Lib "spllib/str.spl")
	:[]

put_name n (CL (CInFun i (InFun "" f)) (K [])) = CL (CInFun i (InFun n f)) (K [])
put_name n o = o

get_code n (Fun c _) = put_name n c
get_code n (Lib f) = do_load (CStr f:[]) get_codes
get_type (Fun _ t) = t
get_type (Lib f) =
	case check0 $ get_code "" (Lib f) of
		SPL.Check3.P (ur, t)|M.null ur -> t
		SPL.Check3.P (ur, _) -> error "get_type"
		SPL.Check3.N i e -> error "get_type"
		

get_codes = M.mapWithKey get_code base
get_types = M.map get_type base

-- native functions
do_sum (CNum a:CNum b:[]) e = CNum (a+b)
do_sum o e = error ("do_sum"++show o)

do_less (CNum a:CNum b:[]) e = CBool (a < b)
do_eq (CNum a:CNum b:[]) e = CBool (a == b)
do_not (CBool b:[]) e = CBool (not b)

do_incr (CNum a:[]) e = CL (CVal "sum") (K [CNum a, CNum 1])
do_incr o e = error ("do_incr"++show o)

do_join1 (a:CList b:[]) e = CList (a:b)
do_join1 o e = error $ show o
do_concat (CList a:CList b:[]) e = CList (a++b)

do_head (CList a:[]) e = head a
do_tail (CList a:[]) e = CList (tail a)
do_reverse (CList a:[]) e = CList (reverse a)
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

do_foldr (f:b:CList []:[]) e = b
do_foldr (f:b:CList (a:as):[]) e =
	eval (CL f (K [a, do_foldr (f:b:CList as:[]) e])) e
	
do_load (CStr f:[]) e =
  unsafePerformIO $
  do
    str <- readFile f
    return $ case SPL.Parser.parse str of
      SPL.Parser.P _ i p ->
				let c = ffi_apply $ compile p in
					case check0 c of
						SPL.Check3.P (ur, _)|M.null ur -> eval c e
						SPL.Check3.P (ur, _) -> error "load error1"
						SPL.Check3.N i e -> error ("load error: "++e)
      SPL.Parser.N i -> error "load error3"

do_out (CStr s:[]) e =
	unsafePerformIO $
	do
		putStrLn s
		return (CNum 0)

do_str_concat (CStr s1:CStr s2:[]) e =
	CStr $ (++) s1 s2

do_to (CNum n:[]) e =
	CList $ Prelude.map CNum $ take n $ enumFrom 0

do_mod (CNum n1:CNum n2:[]) e =
	CNum $ mod n1 n2

do_div (CNum n1:CNum n2:[]) e =
	CNum $ div n1 n2

do_or (CBool n1:CBool n2:[]) e =
	CBool $ (||) n1 n2

do_map (f:CList []:[]) e = CList []
do_map (f:CList (a:as):[]) e =
	case do_map (f:CList as:[]) e of
		CList l -> CList $ (:) (eval (CL f (K [a])) e) l
		_ -> error "do_map"

do_if (CBool True:t:_:[]) e = eval (CL t (K [CVal "go"])) e
do_if (CBool False:_:el:[]) e = eval (CL el (K [CVal "go"])) e
do_if o e = error $ "if: "++show o

ffi_apply (CDebug _ c) = ffi_apply c -- it is partly like r_d from Compile mod
ffi_apply (CStruct m) = CStruct $ M.map ffi_apply m

ffi_apply (CL (CDebug _ (CVal "ffi")) (K [CDebug _ (CStr t), CDebug _ (CStr "str_concat")])) =
	CL (CInFun 2 (InFun t SPL.Lib.Str.do_concat)) (K [])
ffi_apply (CL c (K p)) =
	CL (ffi_apply c) (K (Prelude.map ffi_apply p))
ffi_apply (CL c (S a)) =
	CL (ffi_apply c) (S a)
ffi_apply (CL c (W a)) =
	CL (ffi_apply c) (W (Prelude.map (\(a,b) -> (a, ffi_apply b)) a))
ffi_apply (CL c (D n)) =
	CL (ffi_apply c) (D n)
ffi_apply (CL c R) =
	CL (ffi_apply c) R
ffi_apply (CL c L) =
	CL (ffi_apply c) L
ffi_apply o = o

