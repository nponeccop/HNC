{-# LANGUAGE GADTs, DeriveFunctor, DeriveFoldable #-}
module HN.Optimizer.Node (node, argNode, DefinitionNode(..), Node(..), dnSuccessors, ExpressionFunctor(..), ExpressionFix) where

import Compiler.Hoopl
import Data.Functor.Foldable
import qualified Data.Foldable as F
import HN.Intermediate (Const)

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
	Application a b -> concat $ a : b 
	Constant _ -> []
	Atom a -> [a]

node l dn = mkFirst (Entry l) <*> mkLast (Exit dn)
argNode label = node label ArgNode

data ExpressionFunctor a 
    =   Application a [a]
    |   Atom Label
    |   Constant Const deriving (Functor, F.Foldable, Eq, Show)

data DefinitionNode
	= LetNode [Label] ExpressionFix
	| ArgNode
	| LibNode
	
type ExpressionFix = Fix ExpressionFunctor

