module Main where

import SPL.Compiler
import SPL.Top
import SPL.Check3
import SPL.Interpretator
import SPL.Parser2
import Data.Map as M

--s = "{f a b *a:sum 1 2}"
s = "sum 1"

recompile c r =
	show r

res =
	case parse s of
		SPL.Parser2.P _ i p _ ->
			let c = compile p in
				case check2 c of
					SPL.Check3.P (r, ur, a)|M.null ur -> recompile c r
					SPL.Check3.P (_, u2, a) -> error ("check0 returned saved vars: "++show u2++"\n"++show s)
					SPL.Check3.N i e -> "type error: "
		SPL.Parser2.N i _ ->
			"parser error"


main = putStrLn $ s++"\n\n"++(show Main.res)

