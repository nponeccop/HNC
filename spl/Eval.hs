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
		Sdep f -> eval f c -- is it possibe to get dependence without checking result of "get" ?
		v -> v

eval a@(Snum n) c =
	a

eval (Sfun a@(Serr n) p) c =
	a

eval (Sfun a@(Sn n) p) c =
	eval (Sfun (eval a c) p) c

eval (Sfun (Sfun f p1) p2) c =
	eval (Sfun f (p1++p2)) c

eval (Sfun (Slambda N f p1) p2) c =
	eval (Sfun (Sfun f p1) p2) c

eval (Sfun (Slambda L f p1) p2) c =
	eval (Sfun (add_to_last (Sfun f p1) p2) []) c

eval a@(Sfun (Srun n i (Fun f)) p) c =
	(case length p of
		l|((-1) == i)||(i == l) -> (f (Prelude.map (\p -> eval p c) p) c)
		l|l < i -> a
		l|l > i -> Serr ("too_many_params for "++n++": "++(foldr1 (\x y -> x++"|"++y) (Prelude.map show p))++""))

eval a@(Slambda m f p) c =
	a

add_to_last (Sfun f p@(x:xs)) params =
	case last p of
		Sfun f2 p2 -> Sfun f (init p ++ [add_to_last (last p) params])
		o -> Sfun f (p++params)
add_to_last (Sfun f []) params=
	Sfun f params

{- end of eval -}

make_code a = a

