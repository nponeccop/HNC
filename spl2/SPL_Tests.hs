module Main where

import SPL.Interpretator


data T = Ok [Char] | No [Char]
	deriving Show

get_str (Ok s) = s
get_str (No s) = s
is_passed (Ok s) = True
is_passed (No _) = False

test_last = 0
from_i = 0::Int
--to_i = 90::Int
to_i = (-) (length tests) 1

test_res =
	zipWith (\x y -> test x y) [0..] $ case test_last of
		1 -> [tests!!to_i]
		_ -> take (1 + to_i - from_i) $ drop from_i tests

res1 = foldr1 (++) $ map (\r -> (get_str r)++"\n") test_res

res = res1 ++ ("failed: " ++ show (length $ filter (not . is_passed) test_res))

main = putStrLn res

test i (s, r, t) =
	case step s of
		SPL.Interpretator.P (t2, r2)| r == r2 && t == t2 ->
			Ok $ "ok: "++s
--			Ok $ "ok: "++s++"\n    "++r++"\n    "++t
		SPL.Interpretator.P (t2, r2)| r == r2 ->
			No $ show i++"no: "++s++"\n type exp: "++t++"\n type act: "++t2
		SPL.Interpretator.P (t2, r2)| t == t2 ->
			No $ show i++"no: "++s++"\n res exp: "++r++"\n res act: "++r2
		SPL.Interpretator.P (t2, r2) ->
			No $ show i++"no: "++s++"\n type exp: "++t++"\n type act: "++t2
			++"\n res exp: "++r++"\n res act: "++r2
		SPL.Interpretator.N (i, r3)| r3 == r ->
			Ok $ "ok: "++s++" |err"
--			Ok $ ("ok: "++s++" |err\n    "++r)
		SPL.Interpretator.N (i, r3) ->
			No $ show i++"no: "++s++"\n err exp: "++r++"\n err act: "++r3++""


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
	,("(f*f 1)", "CL (CL (CVal \"f\") (K [CNum 1])) (S [\"f\"])", "TT [TT [T \"num\",TU \"a\"],TU \"a\"]")
	,("(_*sum 1,sum ((z*sum 3,length z) _) 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"sum\") (K [CL (CL (CL (CVal \"sum\") (K [CNum 3,CL (CVal \"length\") (K [CVal \"z\"])])) (S [\"z\"])) (K [CVal \"_\"]),CNum 2])])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],T \"num\"]")
	,("(_*to_string,head _)", "CL (CL (CVal \"to_string\") (K [CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],T \"string\"]")
	,("(_*sum 1,head _)", "CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(_*sum (length _) (head _))", "CL (CL (CVal \"sum\") (K [CL (CVal \"length\") (K [CVal \"_\"]),CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(_*sum (head _) (length _))", "CL (CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"length\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(_*sum (head _),sum ((z*sum 3,length z) _) 2)", "CL (CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"sum\") (K [CL (CL (CL (CVal \"sum\") (K [CNum 3,CL (CVal \"length\") (K [CVal \"z\"])])) (S [\"z\"])) (K [CVal \"_\"]),CNum 2])])) (S [\"_\"])", "TT [TD \"list\" [T \"num\"],T \"num\"]")
	,("(join1 (a*sum a)) elist", "CList [CL (CL (CVal \"sum\") (K [CVal \"a\"])) (S [\"a\"])]", "TD \"list\" [TT [T \"num\",T \"num\",T \"num\"]]")
	,("pair", "CL (CInFun 2 InFun \"pair\") (K [])", "TT [TU \"a\",TU \"b\",TD \"pair\" [TU \"a\",TU \"b\"]]")
	,("pair 'abc'", "CL (CInFun 2 InFun \"pair\") (K [CStr \"abc\"])", "TT [TU \"a\",TD \"pair\" [T \"string\",TU \"a\"]]")
	,("pair 'true' 1b", "CPair [CStr \"true\",CBool True]", "TD \"pair\" [T \"string\",T \"boolean\"]")
	,("join1 (pair 1b 'abc'),elist", "CList [CPair [CBool True,CStr \"abc\"]]", "TD \"list\" [TD \"pair\" [T \"boolean\",T \"string\"]]")
	,("join1 (sum 2),join1 (_*sum 1 _),elist", "CList [CL (CInFun 2 InFun \"sum\") (K [CNum 2]),CL (CL (CVal \"sum\") (K [CNum 1,CVal \"_\"])) (S [\"_\"])]", "TD \"list\" [TT [T \"num\",T \"num\"]]")
	,("(_*to_string,sum 2 _)", "CL (CL (CVal \"to_string\") (K [CL (CVal \"sum\") (K [CNum 2,CVal \"_\"])])) (S [\"_\"])", "TT [T \"num\",T \"string\"]")
	,("join1 (_*to_string,sum 2 _),join1 (_*sum 1 _),elist", "type error: expected TD \"list\" [TT [T \"num\",T \"string\"]], actual TD \"list\" [TT [T \"num\",T \"num\"]]", "")
	,("join1 (_*length _),join1 (_*sum 1,length _),elist", "CList [CL (CL (CVal \"length\") (K [CVal \"_\"])) (S [\"_\"]),CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"length\") (K [CVal \"_\"])])) (S [\"_\"])]", "TD \"list\" [TT [TD \"list\" [TU \"a\"],T \"num\"]]")
	,("(_*sum 1 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"_\"])", "TT [TU \"a\",T \"num\"]")
	,("(_*sum 1 2) 1", "CNum 3", "T \"num\"")
	,("(l*l 2) (sum 1)", "CNum 3", "T \"num\"")
	,("(l*l 1) (_*z*sum 1 2)", "CL (CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"_\",\"z\"])) (K [CNum 1])", "TT [TU \"a\",T \"num\"]")
	,("(l*l)", "CL (CVal \"l\") (S [\"l\"])", "TT [TU \"a\",TU \"a\"]")
	,("(l*l) (z*sum 1 2)", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) (S [\"z\"])", "TT [TU \"a\",T \"num\"]")
	,("(l*l 1) (z*sum 1 2)", "CNum 3", "T \"num\"")
	,("incr 3", "CNum 4", "T \"num\"")
	,("{sum 1 2}", "CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) L", "TT [TL,T \"num\"]")
	,("{sum 1 2} go", "CNum 3", "T \"num\"")
	,("iff elist {2}", "CNum 2", "T \"num\"")
	,("iff (join1 (pair 1b {11}),join1 (pair 0b {22}),elist) {33}", "CNum 11", "T \"num\"")
	,("sum 1 2 3", "type error: too many parameters", "")
	,("(debug (sum 1)) 2", "CNum 3", "T \"num\"")
	,("(f*sum (f 2))", "CL (CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CNum 2])])) (S [\"f\"])", "TT [TT [T \"num\",T \"num\"],T \"num\",T \"num\"]") -- check return
	,("(f*debug (sum (f 2)))", "CL (CL (CVal \"debug\") (K [CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CNum 2])])])) (S [\"f\"])", "TT [TT [T \"num\",T \"num\"],T \"num\",T \"num\"]")
	,("((f*debug (sum (f 2))) (sum 1)) 3", "CNum 6", "T \"num\"")
	,("(debug {11}) go", "CNum 11", "T \"num\"")
	,("(z*z 1) ('_*iif (less _ 5) {sum _,_f,sum _ 1} {_})", "type error: check cannot find \"iif\"", "")
	,("('_*iff (join1 (pair (less _ 5) {sum _,_f,sum _ 1}),elist) {_}) 1", "CNum 15", "T \"num\"")
	,("('_*iff (join1 (pair (less _ 2) {_}),elist) {sum (_f,sum _ -1),_f,sum _ -2}) 10", "CNum 55", "T \"num\"")
	,("(h*t*t)", "CL (CVal \"t\") (S [\"h\",\"t\"])", "TT [TU \"a\",TU \"b\",TU \"b\"]")
	,("iff (join1 (pair 0b {elist}),elist) {join1 1,elist}", "CList [CNum 1]", "TD \"list\" [T \"num\"]")
	,("iff (join1 (pair 0b {join1 1,elist}),elist) {elist}", "CList []", "TD \"list\" [T \"num\"]")
	,("(_*join1 (head _))", "CL (CL (CVal \"join1\") (K [CL (CVal \"head\") (K [CVal \"_\"])])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],TD \"list\" [TU \"a\"],TD \"list\" [TU \"a\"]]")
	,("(_*join1 (head _),elist)", "CL (CL (CVal \"join1\") (K [CL (CVal \"head\") (K [CVal \"_\"]),CVal \"elist\"])) (S [\"_\"])", "TT [TD \"list\" [TU \"a\"],TD \"list\" [TU \"a\"]]")
	,("('_*iff (join1 (pair 0b {_}),elist) {(h*t*join1 h,_f,filter (z*less z h) t) (head _) (tail _)})", "CL (CL (CL (CVal \"iff\") (K [CL (CVal \"join1\") (K [CL (CVal \"pair\") (K [CBool False,CL (CVal \"_\") L]),CVal \"elist\"]),CL (CL (CL (CL (CVal \"join1\") (K [CVal \"h\",CL (CVal \"_f\") (K [CL (CVal \"filter\") (K [CL (CL (CVal \"less\") (K [CVal \"z\",CVal \"h\"])) (S [\"z\"]),CVal \"t\"])])])) (S [\"h\",\"t\"])) (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"tail\") (K [CVal \"_\"])])) L])) (S [\"_\"])) R", "TT [TD \"list\" [T \"num\"],TD \"list\" [T \"num\"]]")
	,("(z*(z*sum (head z) 1) (join1 1,elist))", "CL (CL (CL (CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"z\"]),CNum 1])) (S [\"z\"])) (K [CL (CVal \"join1\") (K [CNum 1,CVal \"elist\"])])) (S [\"z\"])", "TT [TU \"a\",T \"num\"]")
	,("('_*iff (join1 (pair (less (length _) 1) {_}),elist) {(h*t*concat (_f,filter (_*less _ h) t),join1 h,_f,filter (_*not,less _ h) t) (head _) (tail _)})", "CL (CL (CL (CVal \"iff\") (K [CL (CVal \"join1\") (K [CL (CVal \"pair\") (K [CL (CVal \"less\") (K [CL (CVal \"length\") (K [CVal \"_\"]),CNum 1]),CL (CVal \"_\") L]),CVal \"elist\"]),CL (CL (CL (CL (CVal \"concat\") (K [CL (CVal \"_f\") (K [CL (CVal \"filter\") (K [CL (CL (CVal \"less\") (K [CVal \"_\",CVal \"h\"])) (S [\"_\"]),CVal \"t\"])]),CL (CVal \"join1\") (K [CVal \"h\",CL (CVal \"_f\") (K [CL (CVal \"filter\") (K [CL (CL (CVal \"not\") (K [CL (CVal \"less\") (K [CVal \"_\",CVal \"h\"])])) (S [\"_\"]),CVal \"t\"])])])])) (S [\"h\",\"t\"])) (K [CL (CVal \"head\") (K [CVal \"_\"]),CL (CVal \"tail\") (K [CVal \"_\"])])) L])) (S [\"_\"])) R", "TT [TD \"list\" [T \"num\"],TD \"list\" [T \"num\"]]")
	,("(_*_,join1 8,join1 9,join1 4,join1 4,join1 5,join1 3,elist) ('_*iff (join1 (pair (less (length _) 1) {_}),elist) {(h*t*concat (_f,filter (_*less _ h) t),join1 h,_f,filter (_*not,less _ h) t) (head _) (tail _)})", "CList [CNum 3,CNum 4,CNum 4,CNum 5,CNum 8,CNum 9]", "TD \"list\" [T \"num\"]")
	,("(f*x*f x)", "CL (CL (CVal \"f\") (K [CVal \"x\"])) (S [\"f\",\"x\"])", "TT [TT [TU \"a\",TU \"b\"],TU \"a\",TU \"b\"]")
	,("(f*x*f x) (sum 1)", "CL (CL (CL (CVal \"f\") (K [CVal \"x\"])) (S [\"f\",\"x\"])) (K [CL (CVal \"sum\") (K [CNum 1])])", "TT [T \"num\",T \"num\"]")
	,("('case*_*iff (case (less (length _) 1) {_},elist) {(h*t*concat (_f case,filter (_*less _ h) t),join1 h,_f case,filter (_*not,less _ h) t) (head _) (tail _)}) (c*e*l*join1 (pair c e) l),join1 8,join1 9,join1 4,join1 4,join1 5,join1 3,elist", "CList [CNum 3,CNum 4,CNum 4,CNum 5,CNum 8,CNum 9]", "TD \"list\" [T \"num\"]")
	,("(case*iff (case (less 1 5) {sum 1 2},elist))", "CL (CL (CVal \"iff\") (K [CL (CVal \"case\") (K [CL (CVal \"less\") (K [CNum 1,CNum 5]),CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) L,CVal \"elist\"])])) (S [\"case\"])", "TT [TT [T \"boolean\",TT [TL,T \"num\"],TD \"list\" [TU \"a\"],TD \"list\" [TD \"pair\" [T \"boolean\",TT [TL,TU \"b\"]]]],TT [TL,TU \"b\"],TU \"b\"]")
	,("(case*iff (case (less 1 5) {sum 1 2},elist)) (w*y*l*join1 (pair w y) l)", "CL (CInFun 2 InFun \"iff\") (K [CList [CPair [CBool True,CL (CL (CVal \"sum\") (K [CNum 1,CNum 2])) L]]])", "TT [TT [TL,T \"num\"],T \"num\"]")
	,("(x*(y*iff (join1 (pair (less x 5) {sum y y}),elist) {5}) ((x*sum 1,sum 2 x) 2))", "CL (CL (CL (CL (CVal \"iff\") (K [CL (CVal \"join1\") (K [CL (CVal \"pair\") (K [CL (CVal \"less\") (K [CVal \"x\",CNum 5]),CL (CL (CVal \"sum\") (K [CVal \"y\",CVal \"y\"])) L]),CVal \"elist\"]),CL (CNum 5) L])) (S [\"y\"])) (K [CL (CL (CL (CVal \"sum\") (K [CNum 1,CL (CVal \"sum\") (K [CNum 2,CVal \"x\"])])) (S [\"x\"])) (K [CNum 2])])) (S [\"x\"])", "TT [T \"num\",T \"num\"]")
	,("(l*(f*filter f l) (x*less 1 x))", "CL (CL (CL (CL (CVal \"filter\") (K [CVal \"f\",CVal \"l\"])) (S [\"f\"])) (K [CL (CL (CVal \"less\") (K [CNum 1,CVal \"x\"])) (S [\"x\"])])) (S [\"l\"])", "TT [TD \"list\" [T \"num\"],TD \"list\" [T \"num\"]]")
	,("f*(flipped*flipped) (x*y*f y x)", "CL (CL (CL (CVal \"flipped\") (S [\"flipped\"])) (K [CL (CL (CVal \"f\") (K [CVal \"y\",CVal \"x\"])) (S [\"x\",\"y\"])])) (S [\"f\"])", "TT [TT [TU \"a\",TU \"b\",TU \"c\"],TU \"b\",TU \"a\",TU \"c\"]")
	,("(f*x*y*(f x) y)", "CL (CL (CL (CVal \"f\") (K [CVal \"x\"])) (K [CVal \"y\"])) (S [\"f\",\"x\",\"y\"])", "TT [TT [TU \"a\",TU \"b\",TU \"c\"],TU \"a\",TU \"b\",TU \"c\"]")
	,("(f*x*y*join1 (f x) y)", "CL (CL (CVal \"join1\") (K [CL (CVal \"f\") (K [CVal \"x\"]),CVal \"y\"])) (S [\"f\",\"x\",\"y\"])", "TT [TT [TU \"a\",TU \"b\"],TU \"a\",TD \"list\" [TU \"b\"],TD \"list\" [TU \"b\"]]")
	,("(f*l*foldr g elist l*g:(x*y*join1 (f x) y))", "CL (CL (CL (CVal \"foldr\") (K [CVal \"g\",CVal \"elist\",CVal \"l\"])) (W [(\"g\",CL (CL (CVal \"join1\") (K [CL (CVal \"f\") (K [CVal \"x\"]),CVal \"y\"])) (S [\"x\",\"y\"]))])) (S [\"f\",\"l\"])", "TT [TT [TU \"a\",TU \"b\"],TD \"list\" [TU \"a\"],TD \"list\" [TU \"b\"]]")
	,("(flip sum*flip:((f*x*y*f x y)))", "CL (CL (CL (CVal \"f\") (K [CVal \"x\",CVal \"y\"])) (S [\"f\",\"x\",\"y\"])) (K [CVal \"sum\"])", "TT [T \"num\",T \"num\",T \"num\"]")
	,("(sum (f,join1 1,elist) (f,join1 0b,elist)*f:(l*sum 1,length l))", "CNum 4", "T \"num\"")
	,("f*sum (f 1),sum (f 0b),f 'aa'", "CL (CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CNum 1]),CL (CVal \"sum\") (K [CL (CVal \"f\") (K [CBool False]),CL (CVal \"f\") (K [CStr \"aa\"])])])) (S [\"f\"])", "TT [TT [TUL [T \"num\",T \"boolean\",T \"string\"],T \"num\"],T \"num\"]")
	,("(z*z z)", "", "")
	,("(sum a b*a:3*b:sum 1 a)", "CNum 7", "T \"num\"")
	,("load 'spl_tests/config.spl'", "CStruct (fromList [(\"clients\",CStruct (fromList [(\"hosts\",CList [CStr \"localhost\",CStr \"localhost2\"]),(\"port\",CNum 2345)])),(\"port\",CNum 1234)])", "TS (fromList [(\"clients\",TS (fromList [(\"hosts\",TD \"list\" [T \"string\"]),(\"port\",T \"num\")])),(\"port\",T \"num\")])")
	,("('l*x*iff (join1 (pair (less n x) {_f (join1 n l) x}),elist) {l}*n:sum (head l) (head,tail l))", "CL (CL (CL (CL (CVal \"iff\") (K [CL (CVal \"join1\") (K [CL (CVal \"pair\") (K [CL (CVal \"less\") (K [CVal \"n\",CVal \"x\"]),CL (CL (CVal \"_f\") (K [CL (CVal \"join1\") (K [CVal \"n\",CVal \"l\"]),CVal \"x\"])) L]),CVal \"elist\"]),CL (CVal \"l\") L])) (W [(\"n\",CL (CVal \"sum\") (K [CL (CVal \"head\") (K [CVal \"l\"]),CL (CVal \"head\") (K [CL (CVal \"tail\") (K [CVal \"l\"])])]))])) (S [\"l\",\"x\"])) R", "TT [TD \"list\" [T \"num\"],T \"num\",TD \"list\" [T \"num\"]]")
	,("('a*b*sum a,sum b,_f 1b b)", "type error: expected T \"num\", actual T \"boolean\"", "")
	,("(r*if 1b {r 1}#r 2)", "CL (CL (CVal \"if\") (K [CBool True,CL (CL (CVal \"r\") (K [CNum 1])) L,CL (CL (CVal \"r\") (K [CNum 2])) L])) (S [\"r\"])", "TT [TT [T \"num\",TU \"a\"],TU \"a\"]")
	,("foldr concat elist,map (a*map mul/a l) l*l:join1 1,join1 2,elist", "CList [CNum 1,CNum 2,CNum 2,CNum 4]", "TD \"list\" [T \"num\"]")
	,("map to_string,join1 1,join1 2,elist", "CList [CStr \"CNum 1\",CStr \"CNum 2\"]", "TD \"list\" [T \"string\"]")
	,("f 2*a:200*f:((b*sum a b)*a:100)", "CNum 102", "T \"num\"")
--	,("f 10*f:('a*b*if (less b a) {1b} {_f incr/a b})/2", "", "")
--	,("{incr:(x*{b:sum x})}.incr 1,.b", "", "")
	]

{-
 -
 -
 - let id x = x in  (id 'b', id 1)
 -
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

