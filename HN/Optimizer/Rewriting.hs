{-# LANGUAGE GADTs, FlexibleContexts #-}
module HN.Optimizer.Rewriting (ListFact, rewriteExpression2) where

import Compiler.Hoopl
import Data.Functor.Foldable
import Data.Maybe
import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import HN.Optimizer.ExpressionRewriter
import Utils

type ListFact = WithTopAndBot DefinitionNode

isDoubleAtomApplication (ApplicationF (Application (Atom _) _) _)= True
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

inlineApplication formalArgs actualArgs f
	= Just . dropR (rewriteExpression $ flip mapUnion f $ mapFromList $ zip formalArgs $ map (PElem . LetNode []) actualArgs)

inlineApplication2 formalArgs actualArgs = process' $ rewriteAtoms $ mapFromList $ zip formalArgs actualArgs where
	rewriteAtoms :: LabelMap ExpressionFix -> Rewrite ExpressionFix
	rewriteAtoms atomMap (Atom a) = mapLookup a atomMap
	rewriteAtoms _ _ = Nothing

rewriteExpression :: FactBase ListFact -> Rewrite ExpressionFix
rewriteExpression = para . phi

phi :: FactBase ListFact -> ExpressionFunctor (ExpressionFix, Maybe ExpressionFix) -> Maybe ExpressionFix
phi _ (ConstantF _) = Nothing
phi f (AtomF a) = do
	([], e) <- processAtom "rewriteExpression" a $ xtrace ("factBase-atom {" ++ show a ++ "}") f
	return e
phi f expr @ (ApplicationF (Atom a, _) bb) = let
	b' = map (uncurry fromMaybe) bb
	in case processAtom "rewriteApplication.Single" a f of
		Nothing -> applyChildRewrites expr
		Just ([], expr) -> Just $ Application expr b'
		Just (args, expr) -> inlineApplication2 args b' expr
phi f xx | isDoubleAtomApplication (fst <$> xx) = rewriteDoubleAtomApplication f $ embed $ fst <$> xx
phi _ expr @ (ApplicationF _ _) = applyChildRewrites expr

processAtom err a f = case lookupFact a f of
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"
	Just Bot -> error $ err ++ ".rewriteExitL.Bot"
	Just (PElem (LetNode args body)) -> Just (args, body)
	_ -> Nothing

rewriteExpression2 :: FactBase ListFact -> Rewrite ExpressionFix
rewriteExpression2 f = process' $ phi2 f . project

phi2 :: FactBase ListFact -> ExpressionFunctor ExpressionFix -> Maybe ExpressionFix
phi2 _ (ConstantF _) = Nothing
phi2 f a @ (AtomF aa) = do
	([], e) <- processAtom "rewriteExpression" aa $ xtrace ("factBase-atom {" ++ show a ++ "}") f
	return e
phi2 f (ApplicationF (Atom a) b') = case processAtom "rewriteApplication.Single" a f of
	Nothing -> Nothing
	Just ([], expr) -> Just $ Application expr b'
	Just (args, expr) -> inlineApplication args b' f expr
phi2 f xx | isDoubleAtomApplication xx = rewriteDoubleAtomApplication f $ embed xx
phi2 _ (ApplicationF _ _) = Nothing
