module Structure where

import Data.Map as M

data ELmod = N | L | SN | SL
	deriving Show

{- eval type -}
data Eval = En [Char] | Enum Int | Ebool Bool
	| Erun [Char] Int Fun
	| Efun Bool Eval [Eval] [Eval]
	| Elambda ELmod Eval [Eval] [Eval]
	| Edep Eval
	| Eif Eval
	| El [Eval]
	| Eset [Char] Eval
	| Eerr [Char]
	deriving Show

data Fun = Fun ([Eval] -> Context -> Eval)
instance Show Fun where
	show (Fun f) = "Fun"

data Context = Context (Map [Char] Eval)
	deriving Show

tv (En s) = s
tvb (Ebool b) = b
tvl (El s) = s

