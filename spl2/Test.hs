
module Main where
import Interpretator

res = foldr1 (++) $ map (++"\n") $ map test $ tests

main = putStrLn res

test (s, r) =
	let c = step s in
	if r == c
	then "ok - " ++ r
	else ("no:\n  res: "++c++"\n  exp: "++r)

tests = [
	("1", "CNum 1")
	]

