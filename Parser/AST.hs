{-# LANGUAGE DeriveFunctor #-}
module Parser.AST where

data Const
	= ConstString String
	| ConstInt    Int
--	| ConstReal   Double
--	| ConstChar   Char
--	| ConstBool   Bool
	deriving Eq

data LetIn a = Let (Definition a) (LetIn a) | In (Expression a)
	deriving (Eq, Show, Functor)

makeLet :: Expression a -> [Definition a] -> LetIn a
makeLet v = foldr Let (In v)

data Definition a
	= Definition a [a] (LetIn a)
	| Assign a (LetIn a)
--	| While Expression LetIn
--	| If Expression LetIn LetIn
	deriving (Eq, Show, Functor)

data Expression a
	=   Application (Expression a) [Expression a]
	|   Atom a
	|   Constant Const
	deriving (Eq, Functor)

instance Show Const where
	show (ConstString x) = show x
	show (ConstInt x) = show x

instance Show a => Show (Expression a) where
	show e = case e of
		Constant c -> show c
		Atom aa -> show aa
		Application a b -> show a ++ concatMap (\b -> ' ' : show b) b

