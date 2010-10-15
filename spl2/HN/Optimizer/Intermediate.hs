module HN.Optimizer.Intermediate where
import Data.Functor

data Expression a
    =   Application (Expression a) [Expression a]
    |   Atom a
    |   Constant Const
    deriving(Eq)

instance Functor Expression where
	fmap f y = case y of
		Atom a -> Atom $ f a
		Application x y -> Application (fmap f x) $ map (fmap f) y
		Constant x -> Constant x


data LetIn = Let Definition LetIn | In (Expression String)
	deriving (Eq,Show)

letValue (Let _ l) = letValue l
letValue (In v) = v

makeLet :: Expression String -> Program -> LetIn
makeLet v w = ml w where
	ml [] = In v
	ml (d : ww) = Let d $ ml ww

insertLet prg (Definition name args (In value)) = Definition name args $ makeLet value prg



data Definition
	=   Definition String [String] LetIn
    deriving(Eq,Show)

type Program = [Definition]

data Const      =   ConstString String
                |   ConstInt    Int deriving (Eq)

instance Show Const where
	show (ConstString x) =	 show x
	show (ConstInt x) = show x


instance Show a => Show (Expression a) where
	show e = case e of
		Constant c -> show c
		Atom aa -> show aa
		Application a b -> show a ++ concatMap (\b -> ' ' : show b) b
