module Main where

import SPL.Types
import SPL.Top

--_in = (CL (CL (CNum 1) (W [("foo",CNum 2)])) (W [("foo",CBool True)]))
-- _in = CL (CL (CVal "sum") (K [CVal "a"])) (S ["a"])
_in = CL (CL (CL (CL (CVal "incr") (S ["x"])) (K [CL (CL (CVal "x") (S ["x"])) (K [CVal "x"])])) (S ["x"])) (K [CNum 5])

res = check0 _in

main = putStrLn $ show Main.res

