module Main where

import Interpretator


data T = Ok [Char] | No [Char]

get_str (Ok s) = s
get_str (No s) = s
is_passed (Ok s) = True
is_passed (No _) = False

test_res = map test $ tests
res1 = foldr1 (++) $ map (\r -> (get_str r)++"\n") test_res

res = res1 ++ ("failed: " ++ show (length $ filter (not . is_passed) test_res))

main = putStrLn res

test (s, r, t) =
	case step s of
		Interpretator.P (t2, r2)| r == r2 && t == t2 ->
			Ok $ "ok: "++s
		Interpretator.P (t2, r2)| r == r2 ->
			No $ "no: "++s++"\n type exp: "++t++"\n type act: "++t2
		Interpretator.P (t2, r2)| t == t2 ->
			No $ "no: "++s++"\n res exp: "++r++"\n res act: "++r2
		Interpretator.P (t2, r2) ->
			No $ "no: "++s++"\n type exp: "++t++"\n type act: "++t2
			++"\n res exp: "++r++"\n res act: "++r2
--			"no: str: " ++ s ++ "\n  type: " ++ t ++ ""
--			"no: str: " ++ s ++ "\n  type: " ++ t ++ "\n  exp: " ++ r ++ "\n  res: " ++ r2
		Interpretator.N (r3, c)| r3 == r ->
			Ok $ ("ok: "++s++" |err")
		Interpretator.N (r3, c) ->
			No $ ("no: "++s++"\n err exp: "++r++"\n err act: "++r3++"")
--			("no: str: "++s++"\n  res: "++r3++"\n  exp: "++r++"\n  code: "++c)

tests = [
	("1" ,"CNum 1" ,"T \"num\"")
--	,("", "parser error", "")
--	,("12" ,"CNum 12" ,"T \"num\"")
--	,("1b" ,"CBool True", "T \"boolean\"")
--	,("'abc'", "CStr \"abc\"", "T \"string\"")
--	,("sum" ,"CL (CInFun 2 InFun \"sum\") (K [])" ,"TT [T \"num\",T \"num\",T \"num\"]")
--	,("(sum 1)", "CL (CInFun 2 InFun \"sum\") (K [CNum 1])", "TT [T \"num\",T \"num\"]")
--	,("sum 2", "CL (CInFun 2 InFun \"sum\") (K [CNum 2])", "TT [T \"num\",T \"num\"]")
--	,("sum 1 2", "CNum 3", "T \"num\"")
--	,("sum 'abc' 2", "type error: expected T \"num\", actual T \"string\"" ,"")
--	,("elist", "CList []", "TD \"list\" [TU \"a\"]")
--	,("join1 1", "CL (CInFun 2 InFun \"join1\") (K [CNum 1])", "TT [TD \"list\" [T \"num\"],TD \"list\" [T \"num\"]]")
--	,("join1 1,elist", "CList [CNum 1]", "TD \"list\" [T \"num\"]")
--	,("join1 1,2", "type error: expected TD \"list\" [T \"num\"], actual T \"num\"", "")
--	,("to_string,sum 2,length,join1 9,join1 8,elist", "CStr \"CNum 4\"", "T \"string\"")
--	,("(sum _,sum 1 2*_)", "CL (CL (CVal \"sum\") (K [CVal \"_\",CL (CVal \"sum\") (K [CNum 1,CNum 2])])) (S [\"_\"])", "TT [T \"num\",T \"num\"]")
--	,("(sum 1,sum _ 2*_)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"sum\") (K [CVal \"_\",CNum 2])])) (S [\"_\"])", "TT [T \"num\",T \"num\"]")
--	,("head,join1 9,elist", "CNum 9", "T \"num\"")
--	,("(head z*z)", "CL (CL (CVal \"head\") (K [CVal \"z\"])) (S [\"z\"])", "TT [TD \"list\" [TU \"a\"],TU \"a\"]")
--	,("(f 1*f)", "CL (CL (CVal \"f\") (K [CNum 1])) (S [\"f\"])", "TT [TT [T \"num\",TU \"_f\"],TU \"_f\"]")
--	,("(sum 1,sum ((sum 3,length z*z) _) 2*_)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"sum\") (K [CL (CL (CL (CVal \"sum\") (K [CNum 3,CL (CVal \"length\") (K [CVal \"z\"])])) (S [\"z\"])) (K [CVal \"_\"]),CNum 2])])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],T \"num\"]")
--	,("(to_string,head _*_)", "CL (CL (CVal \"to_string\") (K [CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],T \"string\"]")
--	,("(sum 1,head _*_)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
--	,("(sum (length _) (head _)*_)", "CL (CL (CVal \"sum\") (K [CL (CVal \"length\") (K [CVal \"_\"]),CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
--	,("(sum (head _) (length _)*_)", "CL (CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"length\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
--	,("(sum (head _),sum ((sum 3,length z*z) _) 2*_)", "CL (CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"sum\") (K [CL (CL (CL (CVal \"sum\") (K [CNum 3,CL (CVal \"length\") (K [CVal \"z\"])])) (S [\"z\"])) (K [CVal \"_\"]),CNum 2])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
--	,("(join1 (sum a*a)) elist", "CList [CL (CL (CVal \"sum\") (K [CVal \"a\"])) (S [\"a\"])]", "TD \"list\" [TT [T \"num\",TT [T \"num\",T \"num\"]]]")
--	,("pair", "CL (CInFun 2 InFun \"pair\") (K [])", "TT [TU \"a\",TU \"b\",TD \"pair\" [TU \"a\",TU \"b\"]]")
--	,("pair 'abc'", "CL (CInFun 2 InFun \"pair\") (K [CStr \"abc\"])", "TT [TU \"a\",TD \"pair\" [T \"string\",TU \"a\"]]")
--	,("pair 'true' 1b", "CPair [CStr \"true\",CBool True]", "TD \"pair\" [T \"string\",T \"boolean\"]")
--	,("join1 (pair 1b 'abc'),elist", "CList [CPair [CBool True,CStr \"abc\"]]", "TD \"list\" [TD \"pair\" [T \"boolean\",T \"string\"]]")
--	,("join1 (sum 2),join1 (sum 1 _*_),elist", "CList [CL (CInFun 2 InFun \"sum\") (K [CNum 2]),CL (CL (CVal \"sum\") (K [CNum 1,CVal \"_\"])) (S [\"_\"])]", "TD \"list\" [TT [T \"num\",T \"num\"]]")
--	,("join1 (to_string,sum 2 _*_),join1 (sum 1 _*_),elist", "type error: expected TD \"list\" [TT [T \"num\",T \"string\"]], actual TD \"list\" [TT [T \"num\",T \"num\"]]", "")
--	,("join1 (length _*_),join1 (sum 1,length _*_),elist", "CList [CL (CL (CVal \"length\") (K [CVal \"_\"])) (S [\"_\"]),CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"length\") (K [CVal \"_\"])])) (S [\"_\"])]", "TD \"list\" [TT [TD \"list\" [TU \"a\"],T \"num\"]]")
--	,("(sum 1 2*_)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"_\"])", "TT [TU \"_\",T \"num\"]")
--	,("(sum 1 2*_) 1", "CNum 3", "T \"num\"")
--	,("(l 2*l) (sum 1)", "CNum 3", "T \"num\"")
--	,("(l 1*l) (sum 1 2*_*z)", "CL (CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"_\",\"z\"])) (K [CNum 1])", "TT [TU \"z\",T \"num\"]")
--	,("(l*l) (sum 1 2*z)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"z\"])", "TT [TU \"z\",T \"num\"]")
--	,("(l 1*l) (sum 1 2*z)", "CNum 3", "T \"num\"")
--
--	,("incr 3", "CNum 4", "T \"num\"")
--	,("(sum 1 2!l)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) L", "TT [TL,T \"num\"]")
--	,("(sum 1 2!l) go", "CNum 3", "T \"num\"")
--	,("iff (elist) (2!l)", "CNum 2", "T \"num\"")
--	,("iff (join1 (pair 1b (11!l)),join1 (pair 0b (22!l)),elist) (33!l)", "CNum 11", "T \"num\"")
--	,("sum 1 2 3", "type error: too many parameters for CVal \"sum\"", "")
--	,("(debug (sum 1)) 2", "CNum 3", "T \"num\"")
--	,("(sum (f 2)*f)", "CL (CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CNum 2])])) (S [\"f\"])", "TT [TT [T \"num\",T \"num\"],TT [T \"num\",T \"num\"]]") -- check return
--	,("(debug (sum (f 2))*f)", "CL (CL (CVal \"debug\") (K [CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CNum 2])])])) (S [\"f\"])", "TT [TT [T \"num\",T \"num\"],TT [T \"num\",T \"num\"]]")
--	,("((debug (sum (f 2))*f) (sum 1)) 3", "CNum 6", "T \"num\"")
--	,("(debug (11!l)) go", "CNum 11", "T \"num\"")
	,("(z 1*z) (if (less _ 5) (sum _,_f,sum _ 1!l) (_!l)*_!r)", "type error: check cannot find \"if\"", "")
	,("(z 1*z) (iff (,join1,elist)!r)", "CNum 15", "")
--	,("(fib 10*fib)", "", "")
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

