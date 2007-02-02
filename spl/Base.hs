module Base where

import Eval
import Structure
import Data.Map as M
import Debug.Trace as D

{- eval -}
fun_eq (Enum n1:Enum n2:[]) c = Ebool (n1 == n2)
fun_eq (Ebool n1:Ebool n2:[]) c = Ebool (n1 == n2)
fun_eq xs c = Eerr ("eq not possible: " ++ show xs)

fun_not (Ebool b:[]) c =
	Ebool (not b)
fun_not xs c = error ("not not possible: " ++ show xs)
fun_less (Enum n1:Enum n2:[]) c =
	Ebool (n1 < n2)
fun_less xs c = error ("less not possible: " ++ show xs)

fun_sum (Enum n1:Enum n2:[]) c = Enum (n1 + n2)
fun_sum xs c = Eerr ("sum not possible: " ++ show xs)
fun_list l c = El l
fun_map (a@(Elambda m f p w):El l:[]) c = El (Prelude.map (\v -> eval (Efun False a [v] []) c) l)
fun_if (a@(Elambda m1 f1 p1 w1):b@(Elambda m2 f2 p2 w2):_c@(Elambda m3 f3 p3 w3):d:[]) c =
	case eval (Efun False a [d] []) c of
		Ebool True -> eval (Efun False b [d] []) c
		Ebool False -> eval (Efun False _c [d] []) c
		o -> o
fun_fst (a:b:[]) c =
	a
fun_head (El a:[]) c =
	head a
fun_tail (El a:[]) c =
	El (tail a)
fun_length (El a:[]) c =
	Enum (length a)
fun_length xs c = Eerr ("length not possible: " ++ show xs)
fun_join (El a:El b:[]) c =
	El (a++b)
fun_join xs c = Eerr ("join not possible: " ++ show xs)

fun_find (a@(Elambda m f p w):El l:[]) c =
	El (Prelude.filter (\p -> tvb (eval (Efun False a [p] []) c)) l)
fun_find xs c = Eerr ("find not possible: " ++ show xs)
fun_count (Enum n:a:[]) c =
	case n of
		0 -> El []
		n -> El (a:tvl (fun_count ((Enum (n-1)):a:[]) c))
fun_dot p@(x:xs) c = eval (Efun False (last p) (init p) []) c
fun_dot xs c = Eerr ("dot not possible: " ++ show xs)

fun_debug xs c =
	D.trace ("DEBUG "++show xs)
	last xs

base = Context (M.fromList [
	("eq", Elambda N (Erun "eq" 2 (Fun fun_eq)) [] [])
	,("t", Ebool True)
	,("f", Ebool False)
	,("sum", Elambda N (Erun "sum" 2 (Fun fun_sum)) [] [])
	,("list", Elambda N (Erun "list" (-1) (Fun fun_list)) [] [])
	,("map", Elambda N (Erun "map" 2 (Fun fun_map)) [] [])
	,("if", Elambda N (Erun "if" 4 (Fun fun_if)) [] []) -- cond, ok_expr, else_expr, input
	,("fst", Elambda N (Erun "fst" 2 (Fun fun_fst)) [] [])

	,("not", Elambda N (Erun "less" 1 (Fun fun_not)) [] [])
	,("less", Elambda N (Erun "less" 2 (Fun fun_less)) [] [])

	,("head", Elambda N (Erun "head" 1 (Fun fun_head)) [] [])
	,("tail", Elambda N (Erun "tail" 1 (Fun fun_tail)) [] [])
	,("length", Elambda N (Erun "length" 1 (Fun fun_length)) [] [])
	,("join", Elambda N (Erun "join" 2 (Fun fun_join)) [] [])

	,("find", Elambda N (Erun "find" 2 (Fun fun_find)) [] [])

	,("dot", Elambda N (Erun "dot" 2 (Fun fun_dot)) [] [])
	,("count", Elambda N (Erun "count" 2 (Fun fun_count)) [] [])

	,("debug", Elambda N (Erun "debug" (-1) (Fun fun_debug)) [] [])
	])


