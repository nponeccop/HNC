module HN.Intermediate where
import SPL.Types

type Program = [Definition]

data Const      =   ConstString String
                |   ConstInt    Int
 --               |   ConstReal   Double
 --               |   ConstChar   Char
 --               |   ConstBool   Bool
                    deriving(Show, Eq)

data Definition
    =   Definition String [String] Expression [Definition]
    deriving(Eq,Show)

data Root
	=	Root Definition deriving(Eq,Show)

data TypedDefinition = TypedDefinition T String [String] Expression [TypedDefinition]

data Expression
    =   Application Expression [Expression]
    |   Atom String
    |   Lambda [String] Expression
    |   Constant Const
    deriving(Eq,Show)

