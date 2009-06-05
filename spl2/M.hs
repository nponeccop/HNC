module Main where

import SPL.Interpretator
import SPL.Parser
import SPL.Compiler
import SPL.Types
import SPL.Top

--_in = (CL (CL (CNum 1) (W [("foo",CNum 2)])) (W [("foo",CBool True)]))
-- _in = CL (CL (CVal "sum") (K [CVal "a"])) (S ["a"])

--s = "(x*sum x,(y*sum y 1) 3)"
s = "(x*y*sum x y) 5 6"

res = a where SPL.Interpretator.P (a, _) = get_type_tree_of_expr s
main = putStrLn $ Main.res

