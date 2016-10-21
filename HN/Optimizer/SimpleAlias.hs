{-# LANGUAGE FlexibleInstances #-}
module HN.Optimizer.SimpleAlias (runB) where

import Compiler.Hoopl
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.ClassyLattice
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.Lattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.Utils

type SAFact = WithTopAndBot ExpressionFix

instance Lattice SAFact where
	dataflowLattice = flatEqLattice "SimpleAlias"

transferB :: DefinitionNode -> FactBase SAFact -> SAFact
transferB (LetNode [] aa @ (Atom _)) _ = PElem aa
transferB (LetNode args (Application aa @ (Atom _) argValues)) _ = pelemJust $ fmap (const aa) . sequence =<< zipWithExactMay f args argValues where
	f a (Atom b) | a == b = Just ()
	f _ _ = Nothing
transferB _ _ = bot

rewriteB :: DefinitionNode -> FactBase SAFact -> Maybe DefinitionNode
rewriteB (LetNode l expr) f = LetNode l <$> rewrite WithChildren (rewriteExpression f) expr
rewriteB _ _ = Nothing

rewriteExpression :: FactBase SAFact -> Rewrite ExpressionFix
rewriteExpression f (Atom a) = justPElem =<< lookupFact a f
rewriteExpression _ _ = Nothing

justPElem (PElem a) = Just a
justPElem _ = Nothing

pelemJust = maybe Bot PElem

runB :: Pass any SAFact
runB = runPassB PassParams
	{ ppConvertFacts = \_ _ -> noFacts
	, ppTransfer = mkBTransfer $ transferExitB transferB
	, ppRewrite = pureBRewrite $ rewriteExitB rewriteB
	}
