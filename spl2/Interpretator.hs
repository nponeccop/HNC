
module Interpretator where

import Parser
import Compiler
import Code

step str =
	case parse str of
	Just (i, p) ->
		case compile p of
		Just c -> show (eval c base)
		Nothing -> "compile error"
	Nothing -> "parser error"

