-- SPL
-- Stream Programming Language

module Main where

-- imports
import Observe
import Data.Map as M
import System.Random as R
import System.Time as T

{- syntax type -}
data Syntax = Sn [Char] | Snum Int | Sbool Bool
	| Srun [Char] Int Fun
	| Sfun Bool Syntax [Syntax]
	| Sblock Syntax
	| Sdep Syntax
	| Sif Syntax
	| Spair Syntax Syntax
	| Sl [Syntax]
	| Serr [Char]
	deriving Show

tv (Sn s) = s
tv (Snum d) = show d
tv (Sfun b s p) = show s
tvb (Sbool b) = b
tvl (Sl s) = s

{- parse -}

data Tokens = Ts1|Ts|Tsn|Tc1|Tc|Td1|Tdpos|Tdmin|Td
	|Tcc Char|Tss [Char]
	|Texpr|Tparams
	deriving Show

-- basic tokens
call::Tokens -> [Char] -> Int -> Maybe (Int, Syntax)
call (Tcc c) s o|o < length s && c == s!!o = Just (1, Sn [s!!o])
call Ts1 s o|o < length s && s!!o `elem` " \t\n" = Just (1, Sn [s!!o])
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
call (Tss "") s o = Just (0, Sn "")
call (Tss (x:xs)) s o =
	p_or [([Tcc x,Tss xs], (\ls vs -> (ls, Sn ((tv (vs!!0))++(tv (vs!!1))))))]
		s o
call Tdpos s o =
	p_or [([Td1,Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Td1],(\ls vs -> (ls, vs!!0)))] -- was [] here
		s o
call Tdmin s o =
	p_or [([Tcc '-',Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)))))]
		s o
call Td s o =
	p_or [([Tdmin],(\ls vs -> (ls, Snum (read (tv (vs!!0)))))),
		([Tdpos],(\ls vs -> (ls, Snum (read (tv (vs!!0))))))]
		s o

-- main tokens
call Texpr s o =
	p_or [
		([Tcc ',',Texpr,Tparams], \ls vs -> (ls, Sfun False (vs!!1) (tvl (vs!!2)))),
		([Tcc '[',Texpr,Tcc ']'], \ls vs -> (ls, Sfun True (vs!!1) [])),
		([Tcc '(',Texpr,Tcc ')'], \ls vs -> (ls, Sblock (vs!!1))),
		([Tcc '{',Texpr,Tcc '}'], \ls vs -> (ls, Sif (vs!!1))),
		([Tc], \ls vs -> (ls, vs!!0)),
		([Tc,Tdpos], \ls vs -> (ls, vs!!0)),
		([Td], \ls vs -> (ls, vs!!0))]
		s o

call Tparams s o =
	p_or [
		([Tsn,Texpr,Tparams], \ls vs -> (ls, Sl ((vs!!1):(tvl (vs!!2))))),
		([Tsn,Texpr], \ls vs -> (ls, Sl ((vs!!1):[]))),
		([], \ls vs -> (ls, Sl []))]
		s o

--call Tif_elem s o =
--	...

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

fun_cmp (Snum n1:Snum n2:[]) c = Sbool (n1 == n2)
fun_cmp (Sbool n1:Sbool n2:[]) c = Sbool (n1 == n2)

fun_incr (Snum n:[]) c = Snum (n+1)
fun_map (a@(Sfun b f p):Sl l:[]) c = Sl (Prelude.map (\v -> eval (Sfun False a [v]) c) l)
fun_count (Snum n:a:[]) c =
	case n of
		0 -> Sl []
		n -> Sl (a:tvl (fun_count ((Snum (n-1)):a:[]) c))
fun_comma p@(x:xs) c = eval (Sfun False (last p) (init p)) c
{-fun_ifc (Snum n1:p2:Spair (Sbool True) p3:[]) c =
	Spair (Sbool True) p3
fun_ifc (n1:p2:Spair (Sbool False) p3:[]) c =
	case p3 of
		Sl (n2:[])|tvb (fun_cmp (n1:n2:[]) c) -> Spair (Sbool True) p2
		Sl (n2:[]) -> Spair (Sbool False) p3
		o -> Serr ("false and not list" ++ show o)-}

fun_if (x:y:z@(Spair (Sbool True) p3):[]) c =
	z
fun_if (x:y:z@(Spair (Sbool False) p3):[]) c =
	case b of
		Sbool True ->
			case y of
				Sfun True f2 p2 -> Spair (Sbool True) (eval (Sfun False y (tvl p3)) c)
				o -> Spair (Sbool True) o
		Sbool False -> z
	where
		b =
			case x of
				Sfun True f1 p1 -> eval (Sfun False x (tvl p3)) c
				o -> fun_cmp (x:(head (tvl p3)):[]) c								-- fix it
fun_if (x:y:z:[]) c =
	Serr ("IF_ERR: " ++ show z)

data Context = Context (Map [Char] Syntax)
base = Context (M.fromList [
	("one", Snum 1)
	,("t", Sbool True)
	,("cmp", Sfun False (Srun "cmp" 2 (Fun fun_cmp)) [])
	,("ln", Sfun False (Srun "lnot" 1 (Fun (\(Sbool b:[]) c -> Sbool (not b)))) [])
	,("la", Sfun False (Srun "land" 2 (Fun (\(Sbool b1:Sbool b2:[]) c -> Sbool (b1 && b2)))) [])
	,("f", Sdep (Sfun True (Sfun False (Sn "ln") [Sn "t"]) []))
	,("me", Sfun False (Srun "more" 2 (Fun (\(Snum a:Snum b:[]) c -> Sbool (a>=b)))) [])
	,("sum", Sfun False (Srun "sum" 2 (Fun (\(Snum n1:Snum n2:[]) c -> Snum (n1+n2)))) [])
	,("incr", Sfun False (Srun "incr" 1 (Fun fun_incr)) [])
	,("decr", Sfun False (Srun "decr" 1 (Fun (\(Snum n:[]) c -> Snum (n-1)))) [])
	,("list", Sfun False (Srun "list" (-1) (Fun (\l c -> Sl l))) [])
	,("map", Sfun False (Srun "map" 2 (Fun fun_map)) [])
	,("count", Sfun False (Srun "count" 2 (Fun fun_count)) [])
	,("comma", Sfun False (Srun "comma" 2 (Fun fun_comma)) [])
	,("if", Sfun False (Srun "if" 3 (Fun fun_if)) [])
	])

get :: [Char] -> Context -> Syntax
get n (Context c) =
	case M.lookup n c of
		Just a -> a
		Nothing -> Serr ("not_found: "++n++"")
put :: [Char] -> Syntax -> Context -> Context
put n e (Context c) =
	Context (M.insert n e c)

eval :: Syntax -> Context -> Syntax

eval (Sn n) c =
	case get n c of
		Sdep f -> eval (Sfun False f []) c -- is it possibe to get dependence without checking result of "get" ?
		v -> v

eval a@(Snum n) c =
	a

eval x@(Spair a b) c =
	x

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
	eval (add_to_last f (p1++p2)) (put "_" (head p2) c)

-- if

eval (Sfun False a@(Sif f) p1) c =
	case eval (add_to_last f [(Spair (Sbool False) (Sl (Prelude.map (\p -> eval p c) p1)))]) (put "_c" (Sfun False a []) c) of
		(Spair (Sbool True) r) -> r
		(Spair (Sbool False) r) -> Serr "can't find case"

eval (Sblock e) c =
	eval e c

eval a@(Sfun True f p) c =
	a

add_to_last (Sfun False f p@(x:xs)) params=
	case last p of
		Sfun False f2 p2 -> Sfun False f (init p ++ [add_to_last (last p) params])
		o -> Sfun False f (p++params)
add_to_last (Sfun False f []) params=
	Sfun False f params

{- end of eval -}

make_code a = a

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
--	,Test ",[,incr] 2" "Snum 3"
--	,Test ",(incr) 2" "Snum 3"
	,Test "[,incr,incr]" "Sfun True (Sfun False (Sn \"incr\") [Sfun False (Sn \"incr\") []]) []"
	,Test ",[,incr,incr] 3" "Snum 5"
	,Test ",map [,incr,incr],list 1 2 3 4 5" "Sl [Snum 3,Snum 4,Snum 5,Snum 6,Snum 7]"
	,Test ",map [,sum 10],list 1 2 3 4 5" "Sl [Snum 11,Snum 12,Snum 13,Snum 14,Snum 15]"
	,Test ",count 3 ,list 10" "Sl [Sl [Snum 10],Sl [Snum 10],Sl [Snum 10]]"
	,Test ",[,incr,sum] 2 3" "Snum 6"
	,Test ",map [,comma 4],count (,incr 2) [,sum 2]" "Sl [Snum 6,Snum 6,Snum 6]"
	,Test "t" "Sbool True"
	,Test "f" "Sbool False"
	,Test ",la t f" "Sbool False"
	,Test ",{,if [,cmp 2] [,sum 10],if 1 [,sum 9]},incr 1" "Snum 12"
	,Test ",[,ln,me 3] 3" "Sbool False"
	,Test ",[,sum _] 2" "Snum 4"
	,Test "(,incr 3)" "Snum 4"
	,Test ",[,sum (,incr 2)] 2" "Snum 5"
	,Test ",cmp 1 1" "Sbool True"
	,Test ",cmp 2 1" "Sbool False"
	,Test ",{,if 1 1,if [,ln,me 1] [,sum (,_c,sum -1 _),_c,sum -2],if 0 1} 10" "Snum 89"
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



