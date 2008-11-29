module SPL.Types (T (..), C (..), CP (..), St (..), InFun (..)) where

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

data St a =
	K [a]
	| S [[Char]]
	| L
	| R
	deriving (Eq, Show)

data CP = -- Code positioned
	CPBool Bool Int
	| CPNum Int Int
	| CPStr [Char] Int
	| CPVal [Char] Int
	| CPL CP (St CP) Int
	| CPInFun Int InFun Int
	| CPInfFun InFun Int
	| CPList [CP] Int
	| CPPair [CP] Int
	deriving (Eq, Show)

data C =
	CBool Bool
	| CNum Int
	| CStr [Char]
	| CVal [Char]
	| CL C (St C)
	| CInFun Int InFun
	| CInfFun InFun
	| CList [C]
	| CPair [C]
	deriving (Eq, Show)



