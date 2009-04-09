module Main where

import SPL.Types
import SPL.Top

--_in = (CL (CL (CNum 1) (W [("foo",CNum 2)])) (W [("foo",CBool True)]))
-- _in = CL (CL (CVal "sum") (K [CVal "a"])) (S ["a"])
_in = CL (CL (CL (CVal "incr") (K [CL (CL (CVal "x") (S ["x"])) (K [CVal "x"])])) (S ["x"])) (K [CNum 5])

s = "(x*incr ((x*x) x)) 5"

res = s++"\n\n"++(show $ check0 _in)

main = putStrLn $ Main.res

