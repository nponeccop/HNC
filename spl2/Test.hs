
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
	,("(_z 1*_z) (if (less _ 5) (sum _,_f,sum _ 1!l) (_!l)*_!r)", "CNum 15")
	,("list 8 9 4 4 5 3", "CNum 15")
	]



