{-# LANGUAGE GADTs #-}
module HN.Optimizer.Node (node, argNode, DefinitionNode(..), Node(..), dnSuccessors) where

import HN.Intermediate
import Compiler.Hoopl

data Node e x where
	Entry :: Label -> Node C O
	Exit :: DefinitionNode -> Node O C

instance NonLocal Node where
	entryLabel (Entry l) = l
 	successors (Exit dn) = dnSuccessors dn

dnSuccessors ArgNode = []
dnSuccessors (LetNode args e) = args ++ exprSuccessors e

exprSuccessors x = case x of
	Application a b -> exprSuccessors a ++ concatMap exprSuccessors b
	Constant _ -> []
	Atom a -> [a]

node l dn = mkFirst (Entry l) <*> mkLast (Exit dn)
argNode label = node label ArgNode

data DefinitionNode
	= LetNode [Label] (Expression Label)
	| ArgNode
	| LibNode
