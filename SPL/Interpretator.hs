{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SPL.Interpretator (SPL.Interpretator.P (..), step, get_type_of_expr, get_code_of_expr, get_code_of_expr2, get_type_debug_of_expr, get_type_tree_of_expr, get_syntax_of_expr) where

import SPL.Parser2
import SPL.Compiler
import SPL.Check3
import SPL.Top
import System.IO.Unsafe
import SPL.Optimizer1
--import Debug.Trace

import Data.Map as M hiding (map, filter)

data P = P ([Char], [Char]) | N (Int, [Char])
	deriving Show

out s o =
	unsafePerformIO $
	do
--		putStrLn s;
		return o

step str =
	case parse $ out "parse" str of
		SPL.Parser2.P _ i p _ ->
			case compile0 $ out "compile" p of
				SPL.Compiler.P c ->
					case check0 $ out "check" c of
						SPL.Check3.P (_, ur, a)|M.null ur -> SPL.Interpretator.P (show a, show $ eval0 $ out "eval" $ remove_cdebug c)
						SPL.Check3.P (_, u2, a) -> error ("check0 returned saved vars: "++show u2++"\n"++show str)
						SPL.Check3.N i e -> SPL.Interpretator.N $ (i, "type error: " ++ e)
				SPL.Compiler.N i e -> SPL.Interpretator.N (i, "compiler: "++e)
		SPL.Parser2.N i _ ->
			SPL.Interpretator.N (i, "parser error")

get_type_of_expr str =
	case parse str of
		SPL.Parser2.P _ i p _ ->
			case compile0 p of
				SPL.Compiler.P c ->
					case check0 c of
						SPL.Check3.P (_, ur, t) -> SPL.Interpretator.P (show t, "")
						SPL.Check3.N i e -> SPL.Interpretator.N $ (i, "type error: " ++ e)
				SPL.Compiler.N i e -> SPL.Interpretator.N (i, "compile: "++e)
		SPL.Parser2.N i _ ->
			SPL.Interpretator.N (i, "parser error")

get_code_of_expr str =
	case parse str of
		SPL.Parser2.P _ i p _ ->
			case compile0 p of
				SPL.Compiler.P c -> SPL.Interpretator.P ("", show $ remove_cdebug c)
				SPL.Compiler.N i e -> SPL.Interpretator.N (i, e)
		SPL.Parser2.N i _ ->
			SPL.Interpretator.N (i, "  "++(take i $ repeat ' ')++"^ parser error")

get_code_of_expr2 str =
	case parse str of
		SPL.Parser2.P _ i p _ ->
			case compile0 p of
				SPL.Compiler.P c -> SPL.Interpretator.P ("", show $ remove_ctyped c)
				SPL.Compiler.N i e -> SPL.Interpretator.N (i, e)
		SPL.Parser2.N i _ ->
			SPL.Interpretator.N (i, "  "++(take i $ repeat ' ')++"^ parser error")

get_type_debug_of_expr str =
	case parse str of
		SPL.Parser2.P _ i p _ ->
			case compile0 p of
				SPL.Compiler.P c ->
					case check2 c of
						SPL.Check3.P (s, ur, a) -> SPL.Interpretator.P ((show $ (opt s, ur, a)), "")
						SPL.Check3.N i e -> SPL.Interpretator.N $ (i, "type error: " ++ e)
				SPL.Compiler.N i e -> SPL.Interpretator.N $ (i, "compile: "++e)
		SPL.Parser2.N i _ ->
			SPL.Interpretator.N (i, "parser error")

get_type_tree_of_expr str =
	case parse str of
		SPL.Parser2.P _ i p _ ->
			case compile0 p of
				SPL.Compiler.P c ->
					case check0 c of
						SPL.Check3.P (ret, ur, a) -> SPL.Interpretator.P (show $ SPL.Compiler.remove_cdebug ret, "")
						SPL.Check3.N i e -> SPL.Interpretator.N $ (i, "type error: " ++ e)
				SPL.Compiler.N i e -> SPL.Interpretator.N (i, "compile: "++e)
		SPL.Parser2.N i _ ->
			SPL.Interpretator.N (i, "parser error")

get_syntax_of_expr str =
	case parse str of
		SPL.Parser2.P _ i p _ -> SPL.Interpretator.P (show p, "")
		SPL.Parser2.N i _ ->
			SPL.Interpretator.N (i, "parser error")

{-comp2 str =
	case parse str of
	Parser.P i p ->
		case compile p of
			c -> show c
	Parser.N i -> "parser error"
-}


