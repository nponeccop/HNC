{-# LANGUAGE DeriveFunctor, DeriveFoldable, LambdaCase, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module HN.Intermediate (Const(..), Definition(..), Expression(..), ASTDefinition, ASTExpression, ASTLetIn, ExpressionList, GType, letWhere, letValue, makeLet, ExpressionF(..), LetIn(..), LetInF(..), Root, DefinitionBase(..), UTermF(..)) where
import qualified Data.Set as S
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Control.Unification

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

data UTermF t v a = UVarF v | UTermF (t a) deriving (Show, Functor, Traversable, Foldable)

type instance Base (UTerm t v) = UTermF t v

instance Functor t => Recursive (UTerm t v) where
	project = \case
		UVar a -> UVarF a
		UTerm a -> UTermF a

instance Functor t => Corecursive (UTerm t v) where
	embed = \case
		UVarF a -> UVar a
		UTermF a -> UTerm a

makeBaseFunctor ''Expression
makeBaseFunctor ''LetIn
