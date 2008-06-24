module Main where

import Interpretator

res = foldr1 (++) $ map (++"\n") $ map test $ tests

main = putStrLn res

test (s, r, t) =
	case step s of
		Interpretator.P (t2, r2)| r == r2 && t == t2 ->
			"ok: "++s
		Interpretator.P (t2, r2)| r == r2 ->
			"no: "++s++"\n type exp: "++t++"\n type act: "++t2
		Interpretator.P (t2, r2)| t == t2 ->
			"no: "++s++"\n res exp: "++r++"\n res act: "++r2
		Interpretator.P (t2, r2) ->
			"no: "++s++"\n type exp: "++t++"\n type act: "++t2
			++"\n res exp: "++r++"\n res act: "++r2
--			"no: str: " ++ s ++ "\n  type: " ++ t ++ ""
--			"no: str: " ++ s ++ "\n  type: " ++ t ++ "\n  exp: " ++ r ++ "\n  res: " ++ r2
		Interpretator.N (r3, c)| r3 == r ->
			("ok: "++s++" |err")
		Interpretator.N (r3, c) ->
			("no: "++s++"\n err exp: "++r++"\n err act: "++r3++"")
--			("no: str: "++s++"\n  res: "++r3++"\n  exp: "++r++"\n  code: "++c)

tests = [
	("1" ,"CNum 1" ,"T \"num\"")
	,("12" ,"CNum 12" ,"T \"num\"")
	,("1b" ,"CBool True", "T \"boolean\"")
	,("'abc'", "CStr \"abc\"", "T \"string\"")
	,("sum" ,"CL (CInFun 2 InFun \"sum\") (K [])" ,"TT [T \"num\",T \"num\",T \"num\"]")
	,("(sum 1)", "CL (CInFun 2 InFun \"sum\") (K [CNum 1])", "TT [T \"num\",T \"num\"]")
	,("sum 2", "CL (CInFun 2 InFun \"sum\") (K [CNum 2])", "TT [T \"num\",T \"num\"]")
	,("sum 1 2", "CNum 3", "T \"num\"")
	,("sum 'abc' 2", "type error: expected T \"num\", actual T \"string\"" ,"")
	,("elist", "CList []", "TD \"list\" [TU \"a\"]")
	,("joina 1", "CL (CInFun 2 InFun \"joina\") (K [CNum 1])", "TT [TD \"list\" [T \"num\"],TD \"list\" [T \"num\"]]")
	,("joina 1,elist", "CList [CNum 1]", "TD \"list\" [T \"num\"]")
	,("to_string,sum 2,length,joina 9,joina 8,elist", "CStr \"CNum 4\"", "T \"string\"")
--	,("(sum _,sum 1 2*_)", "")
--	,("(sum 1,sum _ 2*_)", "")
--	,("head", "")
--	,("head,joina 9,elist", "")
--	,("(head z*z)", "")
--	,("(sum 1,sum ((sum 3,length z*z) _) 2*_)", "")
--	,("(to_string,head _*_)", "")
--	,("(sum 1,head _*_)", "")
--	,("(sum (length _) (head _)*_)", "")
--	,("(sum (head _) (length _)*_)", "")
--	,("(sum (head _),sum ((sum 3,length z*z) _) 2*_)", "")
--		,("pair", "")
--		,("pair 1", "")
--		,("pair 1b 'abc'", "")
--		,("joina (pair 1b 'abc')", "")
--		,("joina (pair 1b 'abc'),elist", "")
--	,("elist", "")
--	,("joina 1,elist", "")
--	,("joina 1", "")
--	,("joina 1,2", "")
--	,("sum 1,mul 4 2", "CNum 9")
--	,("(_z 1*_z) (if (less _ 5) (sum _,_f,sum _ 1!l) (_!l)*_!r)", "CNum 15")
--	,("(_,list 8 9 4 4 5 3*_) (if (is_empty _) (_!l) ((join (_f,filter (not,less h _*_) t),join (list h),_f,filter (less h _*_) t*h*t) (head _) (tail _)!l)*_!r)", "CList [CNum 3,CNum 4,CNum 4,CNum 5,CNum 8,CNum 9]")
	]

{-
 - problems:
 -   multiple if
 -   parameter to the f3 of (,f1,f2,f3)
 -   list without paramaters
 -}

{-

(_,list 8 9 4 4 5 3*_)
if (is_empty _)
    (_!l)
    ((join (_f,filter (not,less h!p) t),join (list h) (_f,filter (less h) t)*h*t) (head _) (tail _)!l)*_
!r



qsort l =
	if is_empty l
	then []
	else (++) (_f$filter (not.less h) t)$join (list t)$_f$filter (less h) t
	     join (_f,filter (not>less h) t),join (list t),_f,filter (less h) t
-}

