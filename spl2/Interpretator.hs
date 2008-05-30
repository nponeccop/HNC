
module Interpretator where

import Parser
import Compiler
import Check
import Code

step str =
	case parse str of
	Parser.P i p ->
		case compile p of
		Compiler.P c ->
			case check_all c of
----			T [a] -> show (eval c base)
--			T a -> "check " ++ show a
--			TT a -> "check " ++ show a
			a -> "check " ++ show a
		Compiler.N -> "compile error"
	Parser.N -> "parser error"

comp2 str = 
	case parse str of
	Parser.P i p ->
		case compile p of
		Compiler.P c -> show c
		Compiler.N -> "compile error"
	Parser.N -> "parser error"

