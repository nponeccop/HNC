{-# LANGUAGE GADTs #-}
module HN.Optimizer.Node (node, argNode, DefinitionNode(..), Node(..), ExpressionFix, PassResult, Pass, MyGraph, MyBlock) where

import Prelude hiding ((<*>))
import Compiler.Hoopl
import Data.Functor.Foldable
import HN.Intermediate

data Node e x where
	Entry :: Label -> Node C O
	Exit :: DefinitionNode -> Node O C

instance NonLocal Node where
	entryLabel (Entry l) = l
	successors (Exit (LetNode _ e)) = cata exprSuccessors e
	successors _ = []

exprSuccessors (AtomF a) = [a]
exprSuccessors x = concat x

node l dn = mkFirst (Entry l) <*> mkLast (Exit dn)
argNode label = node label ArgNode

type ExpressionFix = Expression Label 

data DefinitionNode
	= LetNode [Label] ExpressionFix
	| ArgNode
	| LibNode

type MyGraph = Graph Node C C

type MyBlock = Block Node C C

type PassResult f = (Graph Node C C, FactBase f, MaybeO C f)

type Pass inputFact outputFact = PassResult inputFact -> SimpleFuelMonad (PassResult outputFact)
