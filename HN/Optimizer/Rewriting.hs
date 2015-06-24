{-# LANGUAGE GADTs, FlexibleContexts #-}
module HN.Optimizer.Rewriting (ListFact, rewriteExpression2) where

import Compiler.Hoopl
import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import HN.Optimizer.ExpressionRewriter
import Utils

type ListFact = WithTopAndBot DefinitionNode

isDoubleAtomApplication (Application (Application (Atom _) _) _)= True
isDoubleAtomApplication _ = False
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

rewriteDoubleAtomApplication :: FactBase ListFact -> Rewrite ExpressionFix
rewriteDoubleAtomApplication f (Application (Application (Atom a) b) c) = case processAtom "rewriteApplication.Double.1" a f of
	Nothing -> Nothing
	Just ([], _) -> error "rewriteApplication.double.var"
	Just (outerParams, Atom aOuterBody) -> case processAtom "rewriteApplication.Double.2" aOuterBody f of
		Just (innerParams, innerBody) -> Just $ dropR (inlineApplication2 old new) innerBody where
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
rewriteExpression2 = process' . phi2

phi2 :: FactBase ListFact -> Rewrite ExpressionFix
phi2 _ (Constant _) = Nothing
phi2 f a @ (Atom aa) = do
	([], e) <- processAtom "rewriteExpression" aa $ xtrace ("factBase-atom {" ++ show a ++ "}") f
	return e
phi2 f (Application (Atom a) b') = case processAtom "rewriteApplication.Single" a f of
	Nothing -> Nothing
	Just (args, expr) -> Just $ case args of
		[] -> Application expr b'
		_ -> dropR (deep $ rewriteExpression2 (flip mapUnion f $ mapFromList $ zip args $ map (PElem . LetNode []) b')) expr -- inlineApplication args b' f expr
phi2 f xx | isDoubleAtomApplication xx = rewriteDoubleAtomApplication f xx
phi2 _ (Application _ _) = Nothing
