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
	deriving (Eq,Show)

letValue (Let _ l) = letValue l
letValue (In v) = v

letWhere (Let d l) = d : letWhere l
letWhere (In _) = []

makeLet :: Expression -> Program -> LetIn
makeLet v w = ml w where
	ml [] = In v
	ml (d : ww) = Let d $ ml ww

data Definition
	=   Definition String [String] LetIn
    deriving(Eq,Show)

data Root
	=	Root Definition deriving(Eq,Show)

data Expression
    =   Application Expression [Expression]
    |   Atom String
    |   Lambda [String] Expression
    |   Constant Const
    deriving(Eq,Show)
