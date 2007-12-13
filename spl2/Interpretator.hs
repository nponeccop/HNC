
module Interpretator where

import Parser
import Compiler
import Code

step str =
	case parse str of
	Parser.P i p ->
		case compile p of
		Compiler.P c -> show (eval c base)
		Compiler.N -> "compile error"
	Parser.N -> "parser error"

