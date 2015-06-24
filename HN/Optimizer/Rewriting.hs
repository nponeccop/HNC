{-# LANGUAGE GADTs, FlexibleContexts #-}
module HN.Optimizer.Rewriting (ListFact, rewriteExpression) where

import Compiler.Hoopl
import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import HN.Optimizer.ExpressionRewriter

type ListFact = WithTopAndBot DefinitionNode

{-
   Инлайнинг двойной аппликации

   (foo bar) baz

   hnMain = {
      foo x = {
          f y = sum y x
          f
      }
      (foo bar) baz
   }

   a -- foo
   b -- bar
   c -- baz
   aOuterBody -- f
   outerParams -- x
   innerParams -- y
   innerBody -- sum x y

   На выходе hnMain = sum baz bar
-}

rewriteDoubleAtomApplication b c f aa = case aa of
	([], _) -> error "rewriteApplication.double.var"
	(outerParams, Atom aOuterBody) -> case processAtom "rewriteApplication.Double.2" aOuterBody f of
		Just (innerParams, innerBody) -> dropR (propagateAtomValues f old new) innerBody where
			old = outerParams ++ innerParams
			new = b ++ c
		_ -> error "rewriteApplication.double.fn.Nothing"

propagateAtomValues f formalArgs actualArgs
	= deep $ rewriteExpression $ flip mapUnion f $ mapFromList $ zip formalArgs $ map (PElem . LetNode []) actualArgs

processAtom err a f = case lookupFact a f of
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"
	Just Bot -> error $ err ++ ".rewriteExitL.Bot"
	Just (PElem (LetNode args body)) -> Just (args, body)
	_ -> Nothing

rewriteExpression :: FactBase ListFact -> Rewrite ExpressionFix
rewriteExpression f = process' phi where
	atom err a ff = ff f <$> processAtom err a f
	phi expr = case expr of
		Constant _ -> Nothing
		Atom a -> do
			([], e) <- processAtom "Lone" a f
			return e
		Application (Atom a) b -> atom "SingleApp" a $ rewriteAtomApplication b
		Application (Application (Atom a) b) c -> atom "DoubleApp" a $ rewriteDoubleAtomApplication b c
		Application _ _ -> Nothing

rewriteAtomApplication b f (args, expr) = case args of
		[] -> Application expr b
		_ -> dropR (propagateAtomValues f args b) expr
