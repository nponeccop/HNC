module Eval where

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
		Sdep f -> eval (Sfun False f []) c -- is it possibe to get dependence without checking result of "get" ?
		v -> v

eval a@(Snum n) c =
	a

eval x@(Spair a b) c =
	x

eval (Sfun False (Sfun False f p1) p2) c =
--	D.trace ("lambda.lambda =")
	eval (Sfun False f (p1++p2)) c

eval (Sfun False a@(Sn s) p) c =
--	D.trace ("[name] =")
	eval (Sfun False (eval a c) p) c

eval a@(Sfun False (Srun n i (Fun f)) p) c =
--	D.trace ("srun =")
	(case length p of
		l|((-1) == i)||(i == l) -> (f (Prelude.map (\p -> eval p c) p) c)
		l|l < i -> a
		l|l > i -> Serr ("too_many_params for "++n++": "++(foldr1 (\x y -> x++"|"++y) (Prelude.map show p))++""))

-- True

eval (Sfun False a@(Sfun True f p1) p2) c =
--	D.trace ("lambda =")
	eval (add_to_last f (p1++p2)) (put "_" (eval (head p2) c) c) -- should I eval "_" ?

-- if

eval (Sfun False a@(Sif f) p1) c =
	D.trace ("if (" ++ show p1 ++ ") =")
	z
	where
		z =
			case eval (add_to_last f [(Spair (Sbool False) (Sl (Prelude.map (\p -> eval p c) p1)))]) (put "_c" (Sfun False a []) c) of
				(Spair (Sbool True) r) -> r
				(Spair (Sbool False) r) -> Serr ("can't find case: " ++ show (eval (head p1) c))

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

