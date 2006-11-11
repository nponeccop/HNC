
{- syntax
call function:

,filter [,more 14] ,map square ,list 1 2 3 4 5

-}

module E1 (parse) where

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

fun_map (Sfun True f p:Sl l:[]) c = Sl (Prelude.map (\e -> eval (Sfun True f (p++[e])) c) l)

data Context = Context (Map [Char] Syntax)
base = Context (M.fromList [
	("one", Snum 1),
	("sum", Sfun False (Srun "sum" 2 (Fun (\(Snum n1:Snum n2:[]) c -> Snum (n1+n2)))) []),
	("incr", Sfun False (Srun "incr" 1 (Fun (\(Snum n:[]) c -> Snum (n+1)))) []),
	("list", Sfun False (Srun "list" (-1) (Fun (\l c -> Sl l))) []),
	("map", Sfun False (Srun "map" 2 (Fun fun_map)) [])])

get :: [Char] -> Context -> Syntax
get n (Context c) =
	case M.lookup n c of
		Just a -> a
		Nothing -> Sn ("not_found ``"++n++"''")
put :: [Char] -> Syntax -> Context -> Context
put n e (Context c) =
	Context (M.insert n e c)

eval :: Syntax -> Context -> Syntax

eval (Sn n) c =
	get n c

eval (Snum n) c =
	Snum n

--eval (Sfun False (Sfun True f p1) (p2:ps)) c =
--	eval (Sfun False f p1) (put "_" (eval p2 c) c)

{-eval (Sfun True f (p:ps)) c =
	observe "eval1"
	eval f (put "_" (eval p c) c)

eval (Sfun True f []) c =
	observe "eval2"
	eval f (put "_" (Sn "err4") c)

eval a@(Sfun b (Srun n i (Fun f)) p) c =
	case length p of
		l|((-1) == i)||(i == l) -> observe "eval0a" eval (Sfun False (f p c) []) c
		l|l < i -> observe "eval0b" a
		l|l > i -> Serr ("too_many_params for "++n++" ``"++(foldr1 (\x y -> x++"|"++y) (Prelude.map show p))++"''")

eval (Sfun False (Sfun b1 f1 p1) p2) c =
	observe "eval3"
	eval (Sfun b1 f1 (p1++p2)) c

eval (Sfun False e p) c =
	observe "eval4"
	eval (Sfun False (eval e c) (Prelude.map (\e -> eval e c) p)) c-}

eval (Sfun True (Sfun False e p1) []) c =
	Sfun True (Sfun False e p1) []

eval (Sfun True f []) c =
	eval f c

eval (Sfun True f (x:xs@p2)) c =
	eval f (put "_" x c)

--eval (Sfun True f p1) c =
--	eval f (put "_" (Snum 3) c)

eval a@(Sfun False (Srun n i (Fun f)) p) c =
--	observe ("(Sfun False (Srun "++n)
	(case length p of
		l|((-1) == i)||(i == l) -> (f p c)
		l|l < i -> a
		l|l > i -> Serr ("too_many_params for "++n++" ``"++(foldr1 (\x y -> x++"|"++y) (Prelude.map show p))++"''"))

eval (Sfun False (Sfun False e p1) p2) c =
--	observe "(Sfun False (Sfun False"
	eval (Sfun False e (p1++p2)) c

eval (Sfun False (Sn n) p) c =
--	observe "(Sfun False"
	eval (Sfun False (eval (Sn n) c) (Prelude.map (\e -> eval e c) p)) c

{- end of eval -}

make_code a = a

run s =
	case parse s of
		Just (i,v) -> Just (eval (make_code v) base)
		Nothing -> Nothing

-- ",incr 7"
--str = ",[incr] 7"
--	",[,incr,incr _] 7"
--	",map incr ,list 1 2 3 4 5"
str = ",map [,incr _] ,list 1 2 3 4 5"
--str = ",[incr] 7"

-- Sfun f1 [(Sfun f2 [p])]
f f1 f2 p = f1 (f2 p)
-- (a -> b) -> (c -> a) -> c -> b
s = parse str
--m = case parse str of {Just (i,v) -> Just (make_code v); Nothing -> Nothing}
e = putStr (foldr1 (\a b -> a++"\n"++b) (Prelude.map (show . run) [str]))


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


