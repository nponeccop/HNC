{-# LANGUAGE GADTs, FlexibleContexts #-}
module HN.Optimizer.Rewriting (ListFact, rewriteExpression) where

import Compiler.Hoopl
import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import HN.Optimizer.ExpressionRewriter

type ListFact = WithTopAndBot DefinitionNode

processAtom err a f = case lookupFact a f of
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"
	Just Bot -> error $ err ++ ".rewriteExitL.Bot"
	Just (PElem (LetNode args body)) -> Just (args, body)
	_ -> Nothing

rewriteExpression :: FactBase ListFact -> Rewrite ExpressionFix
rewriteExpression f = process phi where
	phi expr = case expr of
		Constant _ -> Nothing
		Atom a -> do
			([], e) <- processAtom "Lone" a f
			return e
		Application _ _ -> Nothing

