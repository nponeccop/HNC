module Eval where
import Hugs.Observe
import Structure
import Data.Map as M
import Debug.Trace as D

get :: [Char] -> Context -> Eval
get n (Context c) =
	case M.lookup n c of
		Just a -> a
		Nothing -> Eerr ("not_found: "++n++"")

put :: [Char] -> Eval -> Context -> Context
put n e (Context c) =
	Context (M.insert n e c)


eval :: Eval -> Context -> Eval

eval (En n) c =
	case get n c of
		Edep f -> eval f c -- is it possibe to get dependence without checking result of "get" ?
		v -> v

eval a@(Ebool n) c =
	a

eval a@(Enum n) c =
	a

eval a@(El n) c =
	a

eval a@(Eerr n) c =
	a

eval (Efun b a@(Eerr n) p w) c =
--	error n	
	a

eval (Efun b a@(En n) p w) c =
	eval (Efun b (eval a c) p w) c

eval (Efun b1 (Efun b2 f p1 w1) p2 w2) c =
	eval (Efun b1 f (p1++p2) (w1++w2)) c -- not correct

eval (Efun b2 (Elambda N f p1 w1) p2 w2) c =
	eval (Efun b2 (Efun True f p1 w1) (lval p2 (putw w2 c)) w2) (putall p2 "_" (putw w2 c))

eval (Efun b2 a@(Elambda SN f p1 w1) p2 w2) c =
	eval (Efun b2 (Efun True f p1 w1) (lval p2 (putw w2 c)) w2) (put "_f" a (putall p2 "_" (putw w2 c)))

eval (Efun b2 (Elambda L f p1 w1) p2 w2) c =
	eval (Efun b2 (add_to_last (Efun True f p1 w1) (lval p2 (putw w2 c))) [] w2) (putall p2 "_" (putw w2 c))

eval a@(Efun b (Erun n i (Fun f)) p w) c =
	(case length p of
		l|((-1) == i)||(i == l) -> (f (Prelude.map (\p -> eval p (putw w c)) p) (putw w c))
		l|l < i -> Elambda N a [] w
		l|l > i -> error ("too_many_params for "++n++": "++(foldr1 (\x y -> x++"|"++y) (Prelude.map show p))++""))

eval a@(Elambda m f p w) c =
	a

--putall (En "_":xs) i c =
--	c
putall (x:xs) i c =
	putall xs (i++"_") (put i (eval x c) c)
putall [] i c =
	c

putw (Eset n e:xs) c =
--	D.trace ("putw "++n)
	putw xs (put n (eval e c) c)
putw [] c =
	c

lval l c =
	Prelude.map (\v -> eval v c) l

add_to_last (Efun b f p@(x:xs) w) params =
	case last p of
		Efun False f2 p2 w2 -> Efun b f (init p ++ [add_to_last (last p) params]) w
		o -> Efun b f (p++params) w
add_to_last (Efun b f [] w) params=
	Efun b f params w

{- end of eval -}

make_code a = a

