
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
			True -> show (eval c base)
			False -> "check error"
		Compiler.N -> "compile error"
	Parser.N -> "parser error"

comp2 str = 
	case parse str of
	Parser.P i p ->
		case compile p of
		Compiler.P c -> show c
		Compiler.N -> "compile error"
	Parser.N -> "parser error"

