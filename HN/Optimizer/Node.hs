{-# LANGUAGE GADTs, DeriveFunctor, DeriveFoldable, TypeFamilies, FlexibleInstances #-}
module HN.Optimizer.Node (node, argNode, DefinitionNode(..), Node(..), dnSuccessors, ExpressionFunctor(..), ExpressionFix) where

import Prelude hiding ((<*>), Foldable)
import Compiler.Hoopl
import Data.Functor.Foldable
import qualified Data.Foldable as F
import HN.Intermediate

data Node e x where
	Entry :: Label -> Node C O
	Exit :: DefinitionNode -> Node O C

instance NonLocal Node where
	entryLabel (Entry l) = l
 	successors (Exit dn) = dnSuccessors dn

dnSuccessors (LetNode args e) = args ++ exprSuccessors e
dnSuccessors _ = []

exprSuccessors :: ExpressionFix -> [Label]
exprSuccessors = cata $ \x -> case x of
	ApplicationF a b -> concat $ a : b 
	ConstantF _ -> []
	AtomF a -> [a]

node l dn = mkFirst (Entry l) <*> mkLast (Exit dn)
argNode label = node label ArgNode

data ExpressionFunctor a 
	=   ApplicationF a [a]
	|   AtomF Label
	|   ConstantF Const deriving (Functor, F.Foldable, Eq)
	
type ExpressionFix = Expression Label 
	
type instance Base ExpressionFix = ExpressionFunctor

instance Foldable ExpressionFix where
	project x = case x of
		Atom x -> AtomF x
		Constant x -> ConstantF x
		Application a b -> ApplicationF a b
		
instance Unfoldable ExpressionFix where
	embed x = case x of
		AtomF x -> Atom x
		ConstantF x -> Constant x
		ApplicationF a b -> Application a b

data DefinitionNode
	= LetNode [Label] ExpressionFix
	| ArgNode
	| LibNode
	


