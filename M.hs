module Main where

import SPL.Interpretator

_in = "a 1,b 2~if_a (a*1) {2} z*z:mk_a 1"

res =
	case step _in of
		SPL.Interpretator.P (_, r) -> show r
		_ -> "no"


main = putStrLn $ _in++"\n\n"++(Main.res)

