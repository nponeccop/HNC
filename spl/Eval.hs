module Eval where
import Hugs.Observe
import Structure
import Data.Map as M
import Debug.Trace as D

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
		Sdep f -> eval f c -- is it possibe to get dependence without checking result of "get" ?
		v -> v

eval a@(Sbool n) c =
	a

eval a@(Snum n) c =
	a

eval a@(Sl n) c =
	a

eval a@(Serr n) c =
	a

eval (Sfun b a@(Serr n) p w) c =
--	error n	
	a

eval (Sfun b a@(Sn n) p w) c =
	eval (Sfun b (eval a c) p w) c

eval (Sfun b1 (Sfun b2 f p1 w1) p2 w2) c =
	eval (Sfun b1 f (p1++p2) (w1++w2)) c -- not correct

eval (Sfun b2 (Slambda N f p1 w1) p2 w2) c =
	eval (Sfun b2 (Sfun True f p1 w1) (lval p2 (putw w2 c)) w2) (putall p2 "_" (putw w2 c))

eval (Sfun b2 a@(Slambda SN f p1 w1) p2 w2) c =
	eval (Sfun b2 (Sfun True f p1 w1) (lval p2 (putw w2 c)) w2) (put "_f" a (putall p2 "_" (putw w2 c)))

eval (Sfun b2 (Slambda L f p1 w1) p2 w2) c =
	eval (Sfun b2 (add_to_last (Sfun True f p1 w1) (lval p2 (putw w2 c))) [] w2) (putall p2 "_" (putw w2 c))

eval a@(Sfun b (Srun n i (Fun f)) p w) c =
	(case length p of
		l|((-1) == i)||(i == l) -> (f (Prelude.map (\p -> eval p (putw w c)) p) (putw w c))
		l|l < i -> Slambda N a [] w
		l|l > i -> error ("too_many_params for "++n++": "++(foldr1 (\x y -> x++"|"++y) (Prelude.map show p))++""))

eval a@(Slambda m f p w) c =
	a

--putall (Sn "_":xs) i c =
--	c
putall (x:xs) i c =
	putall xs (i++"_") (put i (eval x c) c)
putall [] i c =
	c

putw (Sset n e:xs) c =
--	D.trace ("putw "++n)
	putw xs (put n (eval e c) c)
putw [] c =
	c

lval l c =
	Prelude.map (\v -> eval v c) l

add_to_last (Sfun b f p@(x:xs) w) params =
	case last p of
		Sfun False f2 p2 w2 -> Sfun b f (init p ++ [add_to_last (last p) params]) w
		o -> Sfun b f (p++params) w
add_to_last (Sfun b f [] w) params=
	Sfun b f params w

{- end of eval -}

make_code a = a

