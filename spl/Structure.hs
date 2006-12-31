module Structure where

import Data.Map as M

data Lmod = N | L | SN | SL
	deriving Show

{- syntax type -}
data Syntax = Sn [Char] | Snum Int | Sbool Bool
	| Srun [Char] Int Fun
	| Sfun Bool Syntax [Syntax] [Syntax]
	| Slambda Lmod Syntax [Syntax] [Syntax]
	| Sdep Syntax
	| Sif Syntax
	| Sl [Syntax]
	| Sset [Char] Syntax
	| Serr [Char]
	deriving Show

data Fun = Fun ([Syntax] -> Context -> Syntax)
instance Show Fun where
	show (Fun f) = "Fun"

data Context = Context (Map [Char] Syntax)
	deriving Show

tv (Sn s) = s
tvb (Sbool b) = b
tvl (Sl s) = s

