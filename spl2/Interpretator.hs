
module Interpretator (Interpretator.P (..), step) where

import Parser
import Compiler
import Check3
import Top
import Debug.Trace

import Data.Map as M hiding (map, filter)

data P = P ([Char], [Char]) | N ([Char], [Char])

step str =
	case parse str of
		Parser.P i p ->
			case compile p of
				Compiler.P c ->
					case check0 c of
						Check3.P (ur, a)|M.null ur -> Interpretator.P (show a, show $ eval0 c)
						Check3.P (u2, a) -> error ("check0 returned saved vars: "++show u2++"\n"++show str)
						Check3.N e -> Interpretator.N $ ("type error: " ++ e, show c)
				Compiler.N ->
					Interpretator.N ("compile error", "")
		Parser.N i ->
--			Interpretator.N ("parser error at "++show i, "")
			Interpretator.N ("  "++(take i $ repeat ' ')++"^ parser error", "")

{-comp2 str = 
	case parse str of
	Parser.P i p ->
		case compile p of
		Compiler.P c -> show c
		Compiler.N -> "compile error"
	Parser.N i -> "parser error"-}

