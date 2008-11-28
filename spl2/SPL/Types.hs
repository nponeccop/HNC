module SPL.Types (T (..), C (..), St (..), InFun (..)) where

import Data.Map

-- type
data T =
	T [Char]
	| TT [T]
	| TU [Char]
	| TV [Char]
	| TD [Char] [T]
	| TL
	deriving (Eq, Show)

-- code
data InFun =
	InFun [Char] ([C] -> Map [Char] C -> C)
instance Show InFun where
	show (InFun s f) = "InFun \""++s++"\""
instance Eq InFun where
	(==) (InFun a f1) (InFun b f2) = (==) a b

data St =
	K [C]
	| S [[Char]]
	| L
	| R
	deriving (Eq, Show)

data C =
	CBool Bool Int
	| CNum Int Int
	| CStr [Char] Int
	| CVal [Char] Int
	| CL C St Int
	| CInFun Int InFun Int
	| CInfFun InFun Int
	| CList [C] Int
	| CPair [C] Int
	deriving (Eq, Show)

