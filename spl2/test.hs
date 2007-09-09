module Main where

import Code

runTest t =
	(show (eval t base))++"\n"

out = foldr1 (++) (map runTest ts)

main = putStr out

