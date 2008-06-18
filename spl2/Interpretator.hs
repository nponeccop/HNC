
module Interpretator (P (..), step) where

import Parser
import Compiler
import Check
import Code

data P = P ([Char], [Char]) | N ([Char], [Char])

step str =
	case parse str of
		Parser.P i p ->
			case compile p of
				Compiler.P c ->
					case check_all c of
						Check.P ([], a) -> Interpretator.P (show a, show $ eval0 c)
						Check.P (u, a) -> Interpretator.P (show a++" |"++show u, show $ eval0 c)
						Check.N e -> Interpretator.N $ ("type error " ++ e, show c)
				Compiler.N ->
					Interpretator.N ("compile error", "")
		Parser.N ->
			Interpretator.N ("parser error", "")

comp2 str = 
	case parse str of
	Parser.P i p ->
		case compile p of
		Compiler.P c -> show c
		Compiler.N -> "compile error"
	Parser.N -> "parser error"

