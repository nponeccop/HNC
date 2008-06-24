module Main where

import Interpretator

res = foldr1 (++) $ map (++"\n") $ map test $ tests

main = putStrLn res

test (s, r) =
	case step s of
		Interpretator.P (t, r2)| r == r2 ->
			"ok: str: " ++ s ++ "\n  type: " ++ t ++ "\n  res: " ++ r2
		Interpretator.P (t, r2) ->
			"no: str: " ++ s ++ "\n  type: " ++ t ++ ""
--			"no: str: " ++ s ++ "\n  type: " ++ t ++ "\n  exp: " ++ r ++ "\n  res: " ++ r2
		Interpretator.N (r3, c) ->
			("no: str: "++s++"\n  res: "++r3++"")
--			("no: str: "++s++"\n  res: "++r3++"\n  exp: "++r++"\n  code: "++c)

tests = [
	("1", "CNum 1")
--	,("12", "CNum 12")
--	,("1b", "CBool True")
--	,("sum", "CL (CInFun \"sum\" 2 InFun) (K [])")
--	,("'abc'", "CStr \"abc\"")
--	,("sum 'abc' 2", "CStr \"abc\"")
--	,("sum 2", "CStr \"abc\"")
--	,("sum 1 2", "CStr \"abc\"")
--	,("(sum 1)", "CL (CInFun 2 InFun) (K [CNum 1])")
	,("to_string,sum 2,length,joina 9,joina 8,elist", "CNum 4")
	,("(sum _,sum 1 2*_)", "")
	,("(sum 1,sum _ 2*_)", "")
	,("head", "")
	,("head,joina 9,elist", "")
	,("(head z*z)", "")
	,("(sum 1,sum ((sum 3,length z*z) _) 2*_)", "")
	,("(to_string,head _*_)", "")
	,("(sum 1,head _*_)", "")
	,("(sum (length _) (head _)*_)", "")
	,("(sum (head _) (length _)*_)", "")
	,("(sum (head _),sum ((sum 3,length z*z) _) 2*_)", "")
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

