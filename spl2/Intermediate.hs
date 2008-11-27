module Intermediate where
import qualified Data.Map as M

type Program = [Definition]

data Const      =   ConstString String
                |   ConstInt    Int
                |   ConstReal   Double
                |   ConstChar   Char
                |   ConstBool   Bool
                    deriving(Show, Eq)

data Definition
    =   Definition String [String] Expression [Definition]
    deriving(Eq,Show)

data Expression
    =   Application Expression [Expression]
    |   Atom String
    |   Lambda [String] Expression
    |   Constant Const
    deriving(Eq,Show)

