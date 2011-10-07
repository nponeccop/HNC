module Main where

import SPL.Interpretator
import Test.SPL


data T = Ok [Char] | No [Char]
	deriving Show

get_str (Ok s) = s
get_str (No s) = s
is_passed (Ok s) = True
is_passed (No _) = False

test_last = 0
ts = tests ++ failingTests
from_i = 0::Int
--to_i = 90::Int
to_i = (-) (length ts) 1

test_res =
	zipWith (\x y -> test x y) [0..] $ case test_last of
		1 -> [ts!!to_i]
		_ -> take (1 + to_i - from_i) $ drop from_i ts

res1 = foldr1 (++) $ map (\r -> (get_str r)++"\n") test_res

res = res1 ++ ("failed: " ++ show (length $ filter (not . is_passed) test_res))

--res = show $ map (\(a,_,_) -> get_type_debug_of_expr a) ts

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

