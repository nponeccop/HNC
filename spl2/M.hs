module Main where

import SPL.Compiler
import SPL.Top
import SPL.Check3
import SPL.Interpretator
import SPL.Parser2
import Data.Map as M

--s = "{f a b *a:sum 1 2}"
s = "sum 1"

res =
	case parse s of
		SPL.Parser2.P _ i p _ ->
			let c = compile p in
				case check2 c of
					SPL.Check3.P (r, ur, a)|M.null ur -> show $ SPL.Compiler.fix_types r SPL.Top.get_types
					SPL.Check3.P (_, u2, a) -> error ("check0 returned saved vars: "++show u2++"\n"++show s)
					SPL.Check3.N i e -> "type error: "
		SPL.Parser2.N i _ ->
			"parser error"


main = putStrLn $ s++"\n\n"++(Main.res)

