module Main where

import Interpretator


data T = Ok [Char] | No [Char]
	deriving Show

get_str (Ok s) = s
get_str (No s) = s
is_passed (Ok s) = True
is_passed (No _) = False

test_res = map test $ case test_last of True -> [last tests]; False -> tests
res1 = foldr1 (++) $ map (\r -> (get_str r)++"\n") test_res

res = res1 ++ ("failed: " ++ show (length $ filter (not . is_passed) test_res))

main = putStrLn res

test (s, r, t) =
	case step s of
		Interpretator.P (t2, r2)| r == r2 && t == t2 ->
			Ok $ "ok: "++s
--			Ok $ "ok: "++s++"\n    "++r++"\n    "++t
		Interpretator.P (t2, r2)| r == r2 ->
			No $ "no: "++s++"\n type exp: "++t++"\n type act: "++t2
		Interpretator.P (t2, r2)| t == t2 ->
			No $ "no: "++s++"\n res exp: "++r++"\n res act: "++r2
		Interpretator.P (t2, r2) ->
			No $ "no: "++s++"\n type exp: "++t++"\n type act: "++t2
			++"\n res exp: "++r++"\n res act: "++r2
		Interpretator.N (r3, c)| r3 == r ->
			Ok $ "ok: "++s++" |err"
--			Ok $ ("ok: "++s++" |err\n    "++r)
		Interpretator.N (r3, c) ->
			No $ "no: "++s++"\n err exp: "++r++"\n err act: "++r3++""

tests = [
	("1" ,"CNum 1" ,"T \"num\"")
	,("", "parser error", "")
	,("12" ,"CNum 12" ,"T \"num\"")
	,("1b" ,"CBool True", "T \"boolean\"")
	,("'abc'", "CStr \"abc\"", "T \"string\"")
	,("sum" ,"CL (CInFun 2 InFun \"sum\") (K [])" ,"TT [T \"num\",T \"num\",T \"num\"]")
	,("(sum 1)", "CL (CInFun 2 InFun \"sum\") (K [CNum 1])", "TT [T \"num\",T \"num\"]")
	,("sum 2", "CL (CInFun 2 InFun \"sum\") (K [CNum 2])", "TT [T \"num\",T \"num\"]")
	,("sum 1 2", "CNum 3", "T \"num\"")
	,("sum 'abc' 2", "type error: expected T \"num\", actual T \"string\"" ,"")
	,("elist", "CList []", "TD \"list\" [TU \"a\"]")
	,("join1 1", "CL (CInFun 2 InFun \"join1\") (K [CNum 1])", "TT [TD \"list\" [T \"num\"],TD \"list\" [T \"num\"]]")
	,("join1 1,elist", "CList [CNum 1]", "TD \"list\" [T \"num\"]")
	,("join1 1,2", "type error: expected TD \"list\" [T \"num\"], actual T \"num\"", "")
	,("to_string,sum 2,length,join1 9,join1 8,elist", "CStr \"CNum 4\"", "T \"string\"")
	,("(_*sum _ 2)", "CL (CL (CVal \"sum\") (K [CVal \"_\",CNum 2])) (S [\"_\"])", "TT [T \"num\",T \"num\"]")
	,("(_*sum _,sum 1 2)", "CL (CL (CVal \"sum\") (K [CVal \"_\",CL (CVal \"sum\") (K [CNum 1,CNum 2])])) (S [\"_\"])", "TT [T \"num\",T \"num\"]")
	,("(_*sum 1,sum _ 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"sum\") (K [CVal \"_\",CNum 2])])) (S [\"_\"])", "TT [T \"num\",T \"num\"]")
	,("head,join1 9,elist", "CNum 9", "T \"num\"")
	,("(z*head z)", "CL (CL (CVal \"head\") (K [CVal \"z\"])) (S [\"z\"])", "TT [TD \"list\" [TU \"a\"],TU \"a\"]")
	,("(f*f 1)", "CL (CL (CVal \"f\") (K [CNum 1])) (S [\"f\"])", "TT [TT [T \"num\",TU \"_f\"],TU \"_f\"]")
	,("(_*sum 1,sum ((z*sum 3,length z) _) 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"sum\") (K [CL (CL (CL (CVal \"sum\") (K [CNum 3,CL (CVal \"length\") (K [CVal \"z\"])])) (S [\"z\"])) (K [CVal \"_\"]),CNum 2])])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],T \"num\"]")
	,("(_*to_string,head _)", "CL (CL (CVal \"to_string\") (K [CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],T \"string\"]")
	,("(_*sum 1,head _)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(_*sum (length _) (head _))", "CL (CL (CVal \"sum\") (K [CL (CVal \"length\") (K [CVal \"_\"]),CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(_*sum (head _) (length _))", "CL (CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"length\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(_*sum (head _),sum ((z*sum 3,length z) _) 2)", "CL (CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"sum\") (K [CL (CL (CL (CVal \"sum\") (K [CNum 3,CL (CVal \"length\") (K [CVal \"z\"])])) (S [\"z\"])) (K [CVal \"_\"]),CNum 2])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(join1 (a*sum a)) elist", "CList [CL (CL (CVal \"sum\") (K [CVal \"a\"])) (S [\"a\"])]", "TD \"list\" [TT [T \"num\",TT [T \"num\",T \"num\"]]]")
	,("pair", "CL (CInFun 2 InFun \"pair\") (K [])", "TT [TU \"a\",TU \"b\",TD \"pair\" [TU \"a\",TU \"b\"]]")
	,("pair 'abc'", "CL (CInFun 2 InFun \"pair\") (K [CStr \"abc\"])", "TT [TU \"a\",TD \"pair\" [T \"string\",TU \"a\"]]")
	,("pair 'true' 1b", "CPair [CStr \"true\",CBool True]", "TD \"pair\" [T \"string\",T \"boolean\"]")
	,("join1 (pair 1b 'abc'),elist", "CList [CPair [CBool True,CStr \"abc\"]]", "TD \"list\" [TD \"pair\" [T \"boolean\",T \"string\"]]")
	,("join1 (sum 2),join1 (_*sum 1 _),elist", "CList [CL (CInFun 2 InFun \"sum\") (K [CNum 2]),CL (CL (CVal \"sum\") (K [CNum 1,CVal \"_\"])) (S [\"_\"])]", "TD \"list\" [TT [T \"num\",T \"num\"]]")
	,("join1 (_*to_string,sum 2 _),join1 (_*sum 1 _),elist", "type error: expected TD \"list\" [TT [T \"num\",T \"string\"]], actual TD \"list\" [TT [T \"num\",T \"num\"]]", "")
{-	,("join1 (_*length _),join1 (_*sum 1,length _),elist", "CList [CL (CL (CVal \"length\") (K [CVal \"_\"])) (S [\"_\"]),CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"length\") (K [CVal \"_\"])])) (S [\"_\"])]", "TD \"list\" [TT [TD \"list\" [TU \"a\"],T \"num\"]]")
	,("(_*sum 1 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"_\"])", "TT [TU \"_\",T \"num\"]")
	,("(_*sum 1 2) 1", "CNum 3", "T \"num\"")
	,("(l*l 2) (sum 1)", "CNum 3", "T \"num\"")
	,("(l*l 1) (_*z*sum 1 2)", "CL (CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"_\",\"z\"])) (K [CNum 1])", "TT [TU \"z\",T \"num\"]")
	,("(l*l) (z*sum 1 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"z\"])", "TT [TU \"z\",T \"num\"]")
	,("(l*l 1) (z*sum 1 2)", "CNum 3", "T \"num\"")
	,("incr 3", "CNum 4", "T \"num\"")
	,("(l!sum 1 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) L", "TT [TL,T \"num\"]")
	,("(l!sum 1 2) go", "CNum 3", "T \"num\"")
	,("iff (elist) (l!2)", "CNum 2", "T \"num\"")
	,("iff (join1 (pair 1b (l!11)),join1 (pair 0b (l!22)),elist) (l!33)", "CNum 11", "T \"num\"")
	,("sum 1 2 3", "type error: too many parameters for CVal \"sum\"", "")
	,("(debug (sum 1)) 2", "CNum 3", "T \"num\"")
	,("(f*sum (f 2))", "CL (CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CNum 2])])) (S [\"f\"])", "TT [TT [T \"num\",T \"num\"],TT [T \"num\",T \"num\"]]") -- check return
	,("(f*debug (sum (f 2)))", "CL (CL (CVal \"debug\") (K [CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CNum 2])])])) (S [\"f\"])", "TT [TT [T \"num\",T \"num\"],TT [T \"num\",T \"num\"]]")
	,("((f*debug (sum (f 2))) (sum 1)) 3", "CNum 6", "T \"num\"")
	,("(debug (l!11)) go", "CNum 11", "T \"num\"")
	,("(z*z 1) (r!_*if (less _ 5) (l!sum _,_f,sum _ 1) (l!_))", "type error: check cannot find \"if\"", "")
	,("(r!_*iff (join1 (pair (less _ 5) (l!sum _,_f,sum _ 1)),elist) (l!_)) 1", "CNum 15", "T \"num\"")
	,("(r!_*iff (join1 (pair (less _ 2) (l!_)),elist) (l!sum (_f,sum _ -1),_f,sum _ -2)) 10", "CNum 55", "T \"num\"")
	,("(_*_,join1 8,join1 9,join1 4,join1 4,join1 5,join1 3,elist) (r!_*iff (join1 (pair (less (length _) 1) (l!_)),elist) (l!(h*t*concat (_f,filter (_*less _ h) t),join1 h,_f,filter (_*not,less _ h) t) (head _) (tail _)))", "CList [CNum 3,CNum 4,CNum 4,CNum 5,CNum 8,CNum 9]", "TD \"list\" [T \"num\"]")
	,("(f*x*f x) (sum 1)", "CL (CL (CL (CVal \"f\") (K [CVal \"x\"])) (S [\"f\",\"x\"])) (K [CL (CVal \"sum\") (K [CNum 1])])", "TT [T \"num\",T \"num\"]")
	,("(r!case*_*iff (case (less (length _) 1) (l!_),elist) (l!(h*t*concat (_f,filter (_*less _ h) t),join1 h,_f,filter (_*not,less _ h) t) (head _) (tail _))) (c*e*l*join1 (pair c e) l),join1 8,join1 9,join1 4,join1 4,join1 5,join1 3,elist", "CList [CNum 3,CNum 4,CNum 4,CNum 5,CNum 8,CNum 9]", "TD \"list\" [T \"num\"]")
	,("(case*iff (case (less 1 5) (l!sum 1 2),elist)) (w*y*l*join1 (pair w y) l)", "CList [CNum 3,CNum 4,CNum 4,CNum 5,CNum 8,CNum 9]", "TD \"list\" [T \"num\"]")-}
	,("(x*(y*iff (join1 (pair (less x 5) (l!sum y y)),elist) (l!5)) ((x*sum 1,sum 2 x) 2))", "CL (CL (CL (CL (CVal \"iff\") (K [CL (CVal \"join1\") (K [CL (CVal \"pair\") (K [CL (CVal \"less\") (K [CVal \"x\",CNum 5]),CL (CL (CVal \"sum\") (K [CVal \"y\",CVal \"y\"])) L]),CVal \"elist\"]),CL (CNum 5) L])) (S [\"y\"])) (K [CL (CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"sum\") (K [CNum 2,CVal \"x\"])])) (S [\"x\"])) (K [CNum 2])])) (S [\"x\"])", "")
	]

test_last = True

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

