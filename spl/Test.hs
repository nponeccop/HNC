module Main where
import Parser
import Base
import Eval

run s =
	case parse s of
		Just (i,v)|i == length s -> Just (eval (make_code v) base)
		Just (i,v)|i < length s -> Nothing
		Nothing -> Nothing

{- test -}
data Test = Test [Char] [Char]
call_test (Test req res) =
	case run req of
		Just r|(show r) == res -> (True, pref ++ "ok: " ++ req)
		Just r -> (False, pref ++ "ERROR: " ++ req ++ "\n"
								++ tab ++ "result: " ++ show r ++ "\n"
								++ tab ++ "  wait: " ++ res ++ "\n"
								++ tab ++ " parse: " ++ (case parse req of Just a -> show a; o -> show o))
		Nothing -> (False, pref ++ "ERROR: Cannot_parse")
	where
		tab = "    "
		pref = "test_"
{- end test -}

tests = [
	Test "1" "Snum 1"
	,Test "t" "Sbool True"
	,Test "f" "Sbool False"
	,Test "sum" "Slambda N (Srun \"sum\" 2 Fun) []"
	,Test ".sum" "Sfun (Srun \"sum\" 2 Fun) []"
	,Test ".sum 1" "Sfun (Srun \"sum\" 2 Fun) [Snum 1]"
	,Test ".sum 1 2" "Snum 3"
	,Test ".list 1 2 3 4 5" "Sl [Snum 1,Snum 2,Snum 3,Snum 4,Snum 5]"
	,Test ".list" "Sl []"
	,Test ".sum 1 .sum 2 .sum 3 4" "Snum 10"
	,Test ",sum 1" "Slambda N (Sn \"sum\") [Snum 1]"
	,Test "(,sum 1)" "Slambda N (Sn \"sum\") [Snum 1]"
	,Test "map" "Slambda N (Srun \"map\" 2 Fun) []"
	,Test ".map (,sum 1).list 1 2 3 4 5" "Sl [Snum 2,Snum 3,Snum 4,Snum 5,Snum 6]"
	,Test ".(.sum 2) 3" "Snum 5"
	,Test ".(sum) 2 3" "Snum 5"
	,Test ".(~sum 2.sum 1) 3" "Snum 6"
	,Test ".map (~sum 1.sum 1).list 1 2 3 4 5" "Sl [Snum 3,Snum 4,Snum 5,Snum 6,Snum 7]"
	,Test ".map (,sum 10).list 1 2 3 4 5" "Sl [Snum 11,Snum 12,Snum 13,Snum 14,Snum 15]"
--	,Test ",count 3 ,list 10" "Sl [Sl [Snum 10],Sl [Snum 10],Sl [Snum 10]]"
--	,Test ",map (.comma 4),count 3 (.sum 2)" "Sl [Snum 6,Snum 6,Snum 6]"
	,Test ".if (,eq 3) (,sum 10) (,sum -3) 3" "Snum 13"
	,Test ".if (,eq 3) (,sum 10) (,sum -3) 4" "Snum 1"
	,Test ".(,if (,eq 3) (,sum 10) ,if (,eq 4) (,sum 11) (,sum -4)) 3" "Snum 13"
	,Test ".if (,eq 3) (,sum 10) (,if (,eq 4) (,sum 11) (,sum -4)) 4" "Snum 15"
	,Test ".fst 3 4" "Snum 3"
	,Test ".if (,eq 3) (,fst 10) (,fst 0) 3" "Snum 10"
{-	,Test ",la t f" "Sbool False"
	,Test ",{,if [,cmp 2] [,sum 10],if 1 [,sum 9]},incr 1" "Snum 12"
	,Test ",[,ln,me 3] 3" "Sbool False"
	,Test ",[,sum _] 2" "Snum 4"
	,Test "(,incr 3)" "Snum 4"
	,Test ",[,sum (,incr 2)] 2" "Snum 5"
	,Test ",cmp 1 1" "Sbool True"
	,Test ",cmp 2 1" "Sbool False"
	,Test ",{,if 1 1,if [,ln,me 1] [,sum (,_c,sum -1 _),_c,sum -2],if 0 1} 10" "Snum 89"-}
	]

{-
-}

main =
	putStr (tests_output ++ "\n" ++ (if tests_sum > 0 then "Failed: " ++ (show tests_sum) else "all passed") ++ "\n")
	where
		tests_sum = foldr1 (\a b -> a + b) (Prelude.map (\(a,_) -> case a of False -> 1; _ -> 0) tests_res)
		tests_output = foldr1 (\a b -> a ++ b)(Prelude.map ((++"\n").snd) tests_res)
		tests_res = Prelude.map call_test tests

-- comments
f1 = not
f2 a b = (>) a b
f3 = (+)

fun1 x = f1 (f2 15 (f3 10 x))
u fa fb z = fa (fb z)
fun2 = (u f1 (u (f2 15) (f3 10)))
fun3 = (u f1 (u (f2 15) (f3 10)))

{-
fun x = (f1 p1a p1b... (f2 p2a... (f3 p3a... (fn pna... x))))
(.) (f1 p1a p1b...) ((.) (f2 p2a...) (fn pna...))
flip : (a -> b) -> (c -> a) -> c -> b
-}
-- ",incr 7"
--str = ",[incr] 7"
--	",[,incr,incr _] 7"
--	",map incr ,list 1 2 3 4 5"
--str = ",map [,incr _] ,list 1 2 3 4 5"
--str = ",[incr] 7"

{-
mk:{,/{+x}'(x,a;(a:(#x)+x),x)}

,join x a ,join x,a:sum x,length x

foldr join ,map reverse ,list [,join x a] ,join (,sum [,length x] x) x

-}

-- Sfun f1 [(Sfun f2 [p])]
-- s = parse str



