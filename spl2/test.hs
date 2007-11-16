module Main where

import Char

c1 (c:cs) = c
a = ord (c1 "0")
b = ord (c1 "9")
abc = map chr [a..b]

main = putStrLn abc


