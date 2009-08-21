module Main where

import SPL.Interpretator
import SPL.Parser2
import SPL.Compiler
import SPL.Types
import SPL.Top

s = "if"

res = parse s

main = putStrLn $ show Main.res

