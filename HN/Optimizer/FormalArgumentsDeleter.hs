{-# LANGUAGE GADTs, TypeFamilies, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances #-}
module HN.Optimizer.FormalArgumentsDeleter (runB) where

import Compiler.Hoopl hiding ((<*>))
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.ArgumentValues (ArgFact, argLattice, AFType)
import HN.Optimizer.Utils

transferB :: DefinitionNode -> FactBase AFType -> AFType
transferB _ _ = bot

rewriteB :: DefinitionNode -> FactBase ArgFact -> Maybe DefinitionNode
rewriteB (LetNode l expr) f = LetNode l <$> process' (rewriteExpression f) expr
rewriteB _ _ = Nothing

convertFact :: ArgFact -> Maybe [WithTopAndBot ExpressionFix]
convertFact ((PElem a, _), _) = Just a
convertFact _ = Nothing

rewriteExpression f (Application aa @ (Atom a) b)
	= fmap (smartApplication aa . map fst) . process deleteArg
		=<< zipExactMay b
		=<< convertFact
		=<< lookupFact a f

rewriteExpression _ _ = Nothing

smartApplication a [] = a
smartApplication a b = Application a b

deleteArg :: Rewrite [(ExpressionFix, WithTopAndBot ExpressionFix)]
deleteArg ((_, PElem _) : tail) = Just tail
deleteArg _ = Nothing

passB = BwdPass
	{ bp_lattice = argLattice
	, bp_transfer = mkBTransfer $ transferMapExitB transferB
	, bp_rewrite = pureBRewrite $ rewriteExitB rewriteB
	}

runB :: Pass ArgFact ArgFact
runB = runPass (analyzeAndRewriteBwd passB) (const . convertFactBase)

instance Functor LabelMap where
	fmap = mapMap
