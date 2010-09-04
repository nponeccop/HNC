module SPL.Types (T (..), C (..), St (..), InFun (..)) where

import Data.Map

-- type
data T =
	T [Char]
	| TT [T]
	| TU [Char]
	| TV [Char]
	| TD [Char] [T]
	| TS (Map [Char] T)
	| TL
	| TUL [T]
	deriving (Eq, Show, Read)

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
	| W [([Char], C)]
	| D [Char]
	| L
	| R
	deriving (Eq, Show)

data C =
	CBool Bool
	| CNum Int
	| CStr [Char]
	| CVal [Char]
	| CL C St
	| CInFun Int InFun
	| CInfFun InFun
	| CSpec [Char]
	| CList [C]
	| CPair [C]
	| CDebug Int C
	| CTyped T C
	| CStruct (Map [Char] C)
	| CF Integer
	deriving (Eq, Show)



