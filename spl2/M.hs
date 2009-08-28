module Main where

import SPL.Interpretator
import SPL.Parser2
import SPL.Compiler
import SPL.Types
import SPL.Top

--s = "{f a b *a:sum 1 2}"
s = "[\n  port:1234\n  clients:[\n    hosts:join1 'localhost',join2 'localhost2',elist\n    port:2345\n  ]\n]\n"

res = parse s

main = putStrLn $ s++"\n\n"++(show Main.res)

