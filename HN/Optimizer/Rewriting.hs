{-# LANGUAGE GADTs, FlexibleContexts #-}
module HN.Optimizer.Rewriting (ListFact, rewriteExpression2) where

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
		Just (innerParams, innerBody) -> dropR (inlineApplication2 old new) innerBody where
			old = outerParams ++ innerParams
			new = b ++ c
		_ -> error "rewriteApplication.double.fn.Nothing"

inlineApplication2 formalArgs actualArgs = process' $ rewriteAtoms $ mapFromList $ zip formalArgs actualArgs where
	rewriteAtoms :: LabelMap ExpressionFix -> Rewrite ExpressionFix
	rewriteAtoms atomMap (Atom a) = mapLookup a atomMap
	rewriteAtoms _ _ = Nothing

processAtom err a f = case lookupFact a f of
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"
	Just Bot -> error $ err ++ ".rewriteExitL.Bot"
	Just (PElem (LetNode args body)) -> Just (args, body)
	_ -> Nothing

rewriteExpression2 :: FactBase ListFact -> Rewrite ExpressionFix
rewriteExpression2 f = process' phi where
	atom err a ff = ff f <$> processAtom err a f
	phi expr = case expr of
		Constant _ -> Nothing
		Atom a -> do
			([], e) <- processAtom "Lone" a f
			return e
		Application (Atom a) b -> atom "SingleApp" a $ rewriteAtomApplication b
		Application (Application (Atom a) b) c -> atom "DoubleApp" a $ rewriteDoubleAtomApplication b c
		Application _ _ -> Nothing

rewriteAtomApplication b' f (args, expr) = case args of
		[] -> Application expr b'
		_ -> dropR (deep $ rewriteExpression2 (flip mapUnion f $ mapFromList $ zip args $ map (PElem . LetNode []) b')) expr
