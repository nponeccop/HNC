{-# LANGUAGE DeriveFunctor, DeriveFoldable, LambdaCase, TypeFamilies #-}
module HN.Intermediate (Const(..), Definition(..), Expression(..), ASTDefinition, ASTExpression, ASTLetIn, ExpressionList, GType, letWhere, letValue, makeLet, ExpressionFunctor(..), LetIn(..), Root, DefinitionBase(..)) where
import qualified Data.Set as S
import Data.Functor.Foldable

import Parser.AST
import SPL.Types (T)

type Program = [Definition String]

type ASTLetIn = LetIn String

letValue (Let _ l) = letValue l
letValue (In v) = v

letWhere (Let d l) = d : letWhere l
letWhere (In _) = []

type ASTDefinition = Definition String

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

