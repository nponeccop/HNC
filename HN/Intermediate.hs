{-# LANGUAGE DeriveFunctor #-}
module HN.Intermediate where
import qualified Data.Set as S
import SPL.Types (T)


type Program = [Definition]

data Const      =   ConstString String
                |   ConstInt    Int
 --               |   ConstReal   Double
 --               |   ConstChar   Char
 --               |   ConstBool   Bool
                    deriving Eq

type ASTLetIn = LetIn String

data LetIn a = Let Definition (LetIn a) | In (Expression a)
	deriving (Eq, Show, Functor)

letValue (Let _ l) = letValue l
letValue (In v) = v

letWhere (Let d l) = d : letWhere l
letWhere (In _) = []

makeLet :: ASTExpression -> Program -> ASTLetIn
makeLet v = foldr Let (In v)

insertLet prg (Definition name args (In value)) = Definition name args $ makeLet value prg

data Definition
	=   Definition String [String] (LetIn String)
	|	Assign String (LetIn String)
--	|	While Expression LetIn
--	|	If Expression LetIn LetIn
    deriving(Eq,Show)

type ASTExpression = Expression String
type ExpressionList  = [ASTExpression]
type GType = (S.Set String, T)

type Root = Program

data Expression a
    =   Application (Expression a) [Expression a]
    |   Atom a
    |   Constant Const
    |   Lambda [a] (Expression a)
    deriving (Eq, Functor)

instance Show Const where
	show (ConstString x) = show x
	show (ConstInt x) = show x

instance Show a => Show (Expression a) where
	show e = case e of
		Constant c -> show c
		Atom aa -> show aa
		Application a b -> show a ++ concatMap (\b -> ' ' : show b) b
