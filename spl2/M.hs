module Main where

import SPL.Interpretator
import SPL.Parser2
import SPL.Compiler
import SPL.Types
import SPL.Top

--s = "{f a b *a:sum 1 2}"
s = "(_*sum 1,sum ((z*sum 3,length z) _) 2)"

res = parse s

main = putStrLn $ show Main.res

