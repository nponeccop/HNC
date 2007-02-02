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

make (Slambda m f p w o) =
	Elambda (make_mod m) (make f) (map make p) (map make w)
	where
		make_mod StN = N
		make_mod StL = L
		make_mod StSN = SN
		make_mod StSL = SL

make (Sset n e o) =
	Eset n (make e)

