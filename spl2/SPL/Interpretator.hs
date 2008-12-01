
module SPL.Interpretator (SPL.Interpretator.P (..), step, get_type_of_expr, get_code_of_expr, get_type_debug_of_expr) where

import SPL.Parser
import SPL.Compiler
import SPL.Check3
import SPL.Top
--import Debug.Trace

import Data.Map as M hiding (map, filter)

data P = P ([Char], [Char]) | N (Int, [Char])

step str =
	case parse str of
		SPL.Parser.P _ i p ->
			let c = compile p in
				case check0 c of
					SPL.Check3.P (ur, a)|M.null ur -> SPL.Interpretator.P (show a, show $ eval0 $ remove_cdebug c)
					SPL.Check3.P (u2, a) -> error ("check0 returned saved vars: "++show u2++"\n"++show str)
					SPL.Check3.N i e -> SPL.Interpretator.N $ (i, "type error: " ++ e)
		SPL.Parser.N i ->
			SPL.Interpretator.N (i, "parser error")

get_type_of_expr str =
	case parse str of
		SPL.Parser.P _ i p ->
				case check0 $ compile p of
					SPL.Check3.P (ur, t) -> SPL.Interpretator.P (show t, "")
					SPL.Check3.N i e -> SPL.Interpretator.N $ (i, "type error: " ++ e)
		SPL.Parser.N i ->
			SPL.Interpretator.N (i, "parser error")

get_code_of_expr str =
	case parse str of
		SPL.Parser.P _ i p ->
			SPL.Interpretator.P (show $ compile p, "")
		SPL.Parser.N i ->
			SPL.Interpretator.N (i, "  "++(take i $ repeat ' ')++"^ parser error")

get_type_debug_of_expr str =
	case parse str of
		SPL.Parser.P _ i p ->
			let c = compile p in
				case check0 c of
					SPL.Check3.P (ur, a) -> SPL.Interpretator.P (show $ (ur, a), "")
					SPL.Check3.N i e -> SPL.Interpretator.N $ (i, "type error: " ++ e)
		SPL.Parser.N i ->
			SPL.Interpretator.N (i, "parser error")

{-comp2 str = 
	case parse str of
	Parser.P i p ->
		case compile p of
			c -> show c
	Parser.N i -> "parser error"
-}


