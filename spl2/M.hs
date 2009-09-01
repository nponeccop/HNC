module Main where

import SPL.Parser2

--s = "{f a b *a:sum 1 2}"
s = "str.concat 1 2"

res = parse s

main = putStrLn $ s++"\n\n"++(show Main.res)

