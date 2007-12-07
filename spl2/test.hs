module Main where

import Char

c1 (c:cs) = c
a = ord (c1 "0")
b = ord (c1 "9")
abc = map chr [a..b]

abc2 =
	show (r 1)
	where
		r a =
			if a < 5
			then a + (r (a + 1))
			else a 

-- (if (less _ 5) (sum _ (_r (sum _ 1))) (_)*_!r) 1

main = putStrLn abc2


