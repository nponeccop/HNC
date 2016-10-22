{-# LANGUAGE DeriveFunctor, DeriveFoldable, LambdaCase, TypeFamilies #-}
module HN.Intermediate where
import qualified Data.Set as S
import Data.Functor.Foldable
import SPL.Types (T)


type Program = [Definition String]

data Const
	= ConstString String
	| ConstInt    Int
--	| ConstReal   Double
--	| ConstChar   Char
--	| ConstBool   Bool
	deriving Eq

type ASTLetIn = LetIn String

data LetIn a = Let (Definition a) (LetIn a) | In (Expression a)
	deriving (Eq, Show, Functor)

letValue (Let _ l) = letValue l
letValue (In v) = v

letWhere (Let d l) = d : letWhere l
letWhere (In _) = []

makeLet :: Expression a -> [Definition a] -> LetIn a
makeLet v = foldr Let (In v)

insertLet prg (Definition name args (In value)) = Definition name args $ makeLet value prg

type ASTDefinition = Definition String

data Definition a
	= Definition a [a] (LetIn a)
	| Assign a (LetIn a)
--	| While Expression LetIn
--	| If Expression LetIn LetIn
	deriving (Eq, Show, Functor)

data DefinitionBase a b
	= DefinitionF a [a] (Expression a) [b]
	| AssignF a (Expression a) [b]
	deriving (Show, Functor)

type instance Base (Definition a) = DefinitionBase a

instance Recursive (Definition a) where
	project x = case x of
		Definition a b l -> DefinitionF a b (letValue l) (letWhere l)
		Assign a l -> AssignF a (letValue l) (letWhere l)

instance Corecursive (Definition a) where
	embed x = case x of
		DefinitionF name args value prg -> Definition name args $ makeLet value prg
		AssignF name value prg -> Assign name $ makeLet value prg

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

data ExpressionFunctor b a
	= ApplicationF a [a]
	| AtomF b
	| ConstantF Const deriving (Functor, Foldable, Eq)

type instance Base (Expression b) = ExpressionFunctor b

instance Recursive (Expression a) where
	project = \case
		Atom x -> AtomF x
		Constant x -> ConstantF x
		Application a b -> ApplicationF a b

instance Corecursive (Expression a) where
	embed = \case
		AtomF x -> Atom x
		ConstantF x -> Constant x
		ApplicationF a b -> Application a b

