module Compile where

import Parser
import Structure

make (Sn n o) =
	En n

make (Snum n o) =
	Enum n

make (Sbool b o) =
	Ebool b

make (Sfun b f p w o) =
	Efun b (make f) (map make p) (map make w)

make (Slambda StN f p w o) =
	Elambda N (make f) (map make p) (map make w)

make (Slambda StL f p w o) =
	Elambda L (make f) (map make p) (map make w)

make (Slambda StSN f p w o) =
	Elambda SN (make f) (map make p) (map make w)

make (Slambda StSL f p w o) =
	Elambda SL (make f) (map make p) (map make w)

make (Sset n e o) =
	Eset n (make e)

