
{- syntax
call function:

,filter [,more 14] ,map square ,list 1 2 3 4 5

-}

module Main where

-- imports
import Observe
import Data.Map as M
import System.Random as R
import System.Time as T

{- parse -}

data Syntax = Sn [Char] | Snum Int | Sbool Bool | Sfun Bool Syntax [Syntax] | Srun [Char] Int Fun | Serr [Char]
	| Sl [Syntax]
	deriving Show
data Tokens = Ts1|Topen|Tclose|Topen2|Tclose2|Topen3|Tclose3
	|Ts|Tsn|Tc1|Tc|Tdot|Tcomma|Tdotcom|Tmin|Td1|Td2|Tdpos|Tdmin|Td
	|Tval|Texpr|Tparams
	deriving Show

tv (Sn s) = s
tv (Snum d) = show d
tv (Sfun b s p) = show s
tvl (Sl s) = s

-- main tokens
call::Tokens -> [Char] -> Int -> Maybe (Int, Syntax)
call Topen s o|o < length s && '(' == s!!o = Just (1, Sn [s!!o])
call Tclose s o|o < length s && ')' == s!!o = Just (1, Sn [s!!o])
call Topen2 s o|o < length s && '[' == s!!o = Just (1, Sn [s!!o])
call Tclose2 s o|o < length s && ']' == s!!o = Just (1, Sn [s!!o])
call Topen3 s o|o < length s && '{' == s!!o = Just (1, Sn [s!!o])
call Tclose3 s o|o < length s && '}' == s!!o = Just (1, Sn [s!!o])
call Tdot s o|o < length s && '.' == s!!o = Just (1, Sn [s!!o])
call Tcomma s o|o < length s && ',' == s!!o = Just (1, Sn [s!!o])
call Tdotcom s o|o < length s && ';' == s!!o = Just (1, Sn [s!!o])
call Tmin s o|o < length s && '-' == s!!o = Just (1, Sn [s!!o])
call Ts1 s o|o < length s && s!!o `elem` " \t" = Just (1, Sn [s!!o])
call Tc1 s o|o < length s && s!!o `elem` "_abcdefghijklmnopqrstuvwxyz" = Just (1, Sn [s!!o])
call Td1 s o| o < length s && s!!o `elem` "1234567890" = Just (1, Sn [s!!o])
call Tc s o =
	p_or [([Tc1,Tc],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Tc1],(\ls vs -> (ls, vs!!0)))]
		s o
call Ts s o =
	p_or [([Ts1,Ts],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Ts1],(\ls vs -> (ls, vs!!0)))]
		s o
call Tsn s o =
	p_or [([Ts1,Tsn],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([],(\ls vs -> (ls, Sn "")))]
		s o
call Td2 s o =
	p_or [([Td1,Td1],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)))))] s o
call Tdpos s o =
	p_or [([Td1,Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Td1],(\ls vs -> (ls, vs!!0)))] -- was [] here
		s o
call Tdmin s o =
	p_or [([Tmin,Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)))))]
		s o
call Td s o =
	p_or [([Tdmin],(\ls vs -> (ls, Snum (read (tv (vs!!0)))))),
		([Tdpos],(\ls vs -> (ls, Snum (read (tv (vs!!0))))))]
		s o

{- s-expressions
S-expr call Texpr s o =
	p_or [([Topen,Texpr,Ts,Tparams,Tclose], \ls vs -> (ls, Sfun (vs!!1) (tvl (vs!!3)))),
				([Tc], \ls vs -> (ls, vs!!0))]
		s o

call Tparams s o =
	p_or [([Texpr,Ts,Tparams], \ls vs -> (ls, Sl ((vs!!0):(tvl (vs!!2))))),
				([Texpr], \ls vs -> (ls, Sl ((vs!!0):[]))),
				([], \ls vs -> (ls, Sl []))]
		s o-}

-- main tokens
call Texpr s o =
	p_or [([Tcomma,Texpr,Tparams], \ls vs -> (ls, Sfun False (vs!!1) (tvl (vs!!2)))),
				([Topen2,Texpr,Tclose2], \ls vs -> (ls, Sfun True (vs!!1) [])),
				([Tc], \ls vs -> (ls, vs!!0)),
				([Tc,Tdpos], \ls vs -> (ls, vs!!0)),
				([Td], \ls vs -> (ls, vs!!0))]
		s o

call Tparams s o =
	p_or [([Tsn,Texpr,Tparams], \ls vs -> (ls, Sl ((vs!!1):(tvl (vs!!2))))),
				([Tsn,Texpr], \ls vs -> (ls, Sl ((vs!!1):[]))),
				([], \ls vs -> (ls, Sl []))]
		s o

call _ _ _ = Nothing

-- parser
p_and :: [Tokens] -> [Char] -> Int -> Maybe (Int, [Syntax])
p_and [] s o = Just (0,[])
p_and (t:ts) s o =
	case call t s o of
		Just (l,v) ->
			(case p_and ts s (o+l) of
				Just (ls,vs) -> Just ((l+ls),(v:vs))  -- Just ls -> Just ((l,v):ls)
				Nothing -> Nothing)
		Nothing -> Nothing

p_or :: [([Tokens], Int -> [Syntax] -> (Int, Syntax))] -> [Char] -> Int -> Maybe (Int, Syntax)
p_or [] _ _ = Nothing
p_or ((ts,f):ls) s o =
  case p_and ts s o of
    Just (ls,vs) -> Just (f ls vs)
    Nothing -> p_or ls s o

parse s =
  p_or [([Texpr],(\ls vs -> (ls, vs!!0)))]
       s 0

{- end of parse -}

{- eval -}
data Fun = Fun ([Syntax] -> Context -> Syntax)
instance Show Fun where
	show (Fun f) = "Fun"

fun_incr (Snum n:[]) c = Snum (n+1)
fun_map (a@(Sfun b f p):Sl l:[]) c = Sl (Prelude.map (\v -> eval (Sfun False a [v]) c) l)

data Context = Context (Map [Char] Syntax)
base = Context (M.fromList [
	("one", Snum 1)
	,("sum", Sfun False (Srun "sum" 2 (Fun (\(Snum n1:Snum n2:[]) c -> Snum (n1+n2)))) [])
	,("incr", Sfun False (Srun "incr" 1 (Fun fun_incr)) [])
	,("list", Sfun False (Srun "list" (-1) (Fun (\l c -> Sl l))) [])
	,("map", Sfun False (Srun "map" 2 (Fun fun_map)) [])
	])

get :: [Char] -> Context -> Syntax
get n (Context c) =
	case M.lookup n c of
		Just a -> a
		Nothing -> Sn ("not_found: "++n++"")
put :: [Char] -> Syntax -> Context -> Context
put n e (Context c) =
	Context (M.insert n e c)

eval :: Syntax -> Context -> Syntax

eval (Sn n) c =
	get n c

eval (Snum n) c =
	Snum n

eval (Sfun False (Sfun False f p1) p2) c =
	eval (Sfun False f (p1++p2)) c

eval (Sfun False a@(Sn s) p) c =
	eval (Sfun False (eval a c) p) c

eval a@(Sfun False (Srun n i (Fun f)) p) c =
	(case length p of
		l|((-1) == i)||(i == l) -> (f (Prelude.map (\p -> eval p c) p) c)
		l|l < i -> a
		l|l > i -> Serr ("too_many_params for "++n++": "++(foldr1 (\x y -> x++"|"++y) (Prelude.map show p))++""))

-- True

eval (Sfun False a@(Sfun True f p1) p2) c =
	case add_to_true a of
		Sfun True f p -> eval (Sfun False f p) (put "_" (head p2) c)
		o -> o

eval a@(Sfun True f p) c =
	a

add_to_true (Sfun True f p) =
	case f of
		Sfun False _ _ -> Sfun True (add_to_last f) p
		o -> Sfun True f (p++[Sn "_"])
	where
		add_to_last (Sfun False f p) =
			case p of
				x:xs ->
					case last p of
						Sfun False f2 p2 -> Sfun False f (init p ++ [add_to_last (last p)])
						o -> Sfun False f (p++[Sn "_"])
				[] -> Sfun False f [Sn "_"]

{- end of eval -}

make_code a = a

run s =
	case parse s of
		Just (i,v) -> Just (eval (make_code v) base)
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
	Test "incr" "Sfun False (Srun \"incr\" 1 Fun) []"
	,Test ",incr" "Sfun False (Srun \"incr\" 1 Fun) []"
	,Test "1" "Snum 1"
	,Test ",incr 1" "Snum 2"
	,Test ",incr 1 2" "Serr \"too_many_params for incr: Snum 1|Snum 2\""
	,Test ",map" "Sfun False (Srun \"map\" 2 Fun) []"
	,Test ",list 1 2 3 4 5" "Sl [Snum 1,Snum 2,Snum 3,Snum 4,Snum 5]"
	,Test ",incr ,incr ,incr 3" "Snum 6"
	,Test ",map incr,list 1 2 3 4 5" "Sl [Snum 2,Snum 3,Snum 4,Snum 5,Snum 6]"
	,Test ",sum 1 2" "Snum 3"
	,Test "[,sum 2]" "Sfun True (Sfun False (Sn \"sum\") [Snum 2]) []"
	,Test ",[,sum 2] 3" "Snum 5"
	,Test ",[incr] 2" "Snum 3"
	,Test "[,incr,incr]" "Sfun True (Sfun False (Sn \"incr\") [Sfun False (Sn \"incr\") []]) []"
	,Test ",[,incr,incr] 3" "Snum 5"
	,Test ",map [,incr,incr],list 1 2 3 4 5" "Sl [Snum 3,Snum 4,Snum 5,Snum 6,Snum 7]"
	]

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



