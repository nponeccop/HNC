
module Main where
import Interpretator

res = foldr1 (++) $ map (++"\n") $ map test $ tests

main = putStrLn res

test (s, r) =
	let c = step s in
	if r == c
	then "ok - " ++ r
	else ("no:\n  str: "++s++"\n  res: "++c++"\n  exp: "++r)

tests = [
	("1", "CNum 1")
	,("12", "CNum 12")
	,("1b", "CBool True")
	,("sum", "CL (CInFun 2 InFun) (K [])")
	,("(sum 1)", "CL (CInFun 2 InFun) (K [CNum 1])")
	,("sum 1 2", "CNum 3")
	,("sum 1,mul 4 2", "CNum 9")
	,("(_z 1*_z) (if (less _ 5) (sum _,_f,sum _ 1!l) (_!l)*_!r)", "CNum 15")
	,("(_,list 8 9 4 4 5 3*_) (if (is_empty _) (_!l) ((join (_f,filter (not,less h _*_) t),join (list h) (_f,filter (less h _*_) t)*h*t) (head _) (tail _)!l)*_!r)", "CList [CNum 3,CNum 4,CNum 4,CNum 5,CNum 8,CNum 9]")
	]

{-
 - problems:
 -   multiple if
 -   parameter to the f3 of (,f1,f2,f3)
 -   list without paramaters
 -}

(_,list 8 9 4 4 5 3*_)
  (if (is_empty _)
    (_!l)
    ((join (_f,filter (not,less h _*_) t),join (list h) (_f,filter (less h _*_) t)*h*t) (head _) (tail _)!l)*_!r
  )
