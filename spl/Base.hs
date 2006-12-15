module Base where

import Eval
import Structure
import Data.Map as M
import Debug.Trace as D

{- eval -}
fun_eq (Snum n1:Snum n2:[]) c = Sbool (n1 == n2)
fun_eq (Sbool n1:Sbool n2:[]) c = Sbool (n1 == n2)

fun_sum (Snum n1:Snum n2:[]) c = Snum (n1 + n2)
fun_list l c = Sl l
fun_map (a@(Slambda m f p):Sl l:[]) c = Sl (Prelude.map (\v -> eval (Sfun a [v]) c) l)
fun_if (a@(Slambda m1 f1 p1):b@(Slambda m2 f2 p2):_c@(Slambda m3 f3 p3):d@(Snum n):[]) c =
	case tvb (eval (Sfun a [d]) c) of
		True -> eval (Sfun b [d]) c
		False -> eval (Sfun _c [d]) c
fun_fst (a:b:[]) c =
	a
{-fun_count (Snum n:a:[]) c =
	case n of
		0 -> Sl []
		n -> Sl (a:tvl (fun_count ((Snum (n-1)):a:[]) c))
fun_comma p@(x:xs) c = eval (Sfun (last p) (init p)) c-}

base = Context (M.fromList [
	("eq", Slambda N (Srun "eq" 2 (Fun fun_eq)) [])
	,("t", Sbool True)
	,("f", Sbool False)
	,("sum", Slambda N (Srun "sum" 2 (Fun fun_sum)) [])
	,("list", Slambda N (Srun "list" (-1) (Fun fun_list)) [])
	,("map", Slambda N (Srun "map" 2 (Fun fun_map)) [])
	,("if", Slambda N (Srun "if" 4 (Fun fun_if)) []) -- cond, ok_expr, else_expr, input
	,("fst", Slambda N (Srun "fst" 2 (Fun fun_fst)) [])
--	,("comma", Sfun (Srun "comma" 2 (Fun fun_comma)) [])
--	,("count", Sfun (Srun "count" 2 (Fun fun_count)) [])
	])


