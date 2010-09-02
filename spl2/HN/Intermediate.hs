module HN.Intermediate where

type Program = [Definition]

data Const      =   ConstString String
                |   ConstInt    Int
 --               |   ConstReal   Double
 --               |   ConstChar   Char
 --               |   ConstBool   Bool
                    deriving(Show, Eq)
--
data LetIn = Let Definition LetIn | In Expression

data Definition
--	=   Definition String [String] LetIn
    =   Definition String [String] Expression [Definition]
    deriving(Eq,Show)

data Root
	=	Root Definition deriving(Eq,Show)

data Expression
    =   Application Expression [Expression]
    |   Atom String
    |   Lambda [String] Expression
    |   Constant Const
    deriving(Eq,Show)
