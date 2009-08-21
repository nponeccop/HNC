module Main where

import SPL.Interpretator
import SPL.Parser2
import SPL.Compiler
import SPL.Types
import SPL.Top

s = "{\n1}"

res = parse s

main = putStrLn $ show Main.res

