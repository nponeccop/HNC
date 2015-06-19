{-# LANGUAGE GADTs #-}
module HN.Optimizer.Rewriting (rewriteExpression, ListFact) where

import Compiler.Hoopl
import qualified Data.Foldable as F
import Data.Functor.Foldable
import Data.Maybe
import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import Utils

type Rewrite a = a -> Maybe a

type ListFact = WithTopAndBot DefinitionNode

dropR :: Rewrite a -> a -> a
dropR a x = fromMaybe x (a x)

isAtomApplication (Application (Atom _) _) = True
isAtomApplication _ = False

rewriteApplication :: ExpressionFix -> [ExpressionFix] -> FactBase ListFact -> Maybe ExpressionFix
rewriteApplication (Application (Atom a) b) c f = case processAtom "rewriteApplication.Double.1" a f of
	Nothing -> Nothing
	Just ([], _) -> error "rewriteApplication.double.var"
	Just (outerParams, Atom aOuterBody) -> case processAtom2 "rewriteApplication.Double.2" (Atom aOuterBody) f of
		Just (innerParams, innerBody) -> ff <$> inlineApplication innerParams c f innerBody where
			ff (Application aa bb) = Application (dropR (inlineApplication outerParams b f) aa) bb
			ff _ = error "rewriteApplication.double.fn.Just.noApp"
		_ -> error "rewriteApplication.double.fn.Nothing"

inlineApplication formalArgs actualArgs f
	= Just . dropR (rewriteExpression $ flip mapUnion f $ mapFromList $ zip formalArgs $ map (PElem . LetNode []) actualArgs)

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
		Nothing -> foo expr
		Just ([], expr) -> Just $ Application expr b'
		Just (args, expr) -> inlineApplication args b' f expr
phi f (ApplicationF (a, _) bb) | isAtomApplication a = rewriteApplication a (map fst bb) f
phi _ expr @ (ApplicationF _ _) = foo expr

foo (ApplicationF (a, _) bb) = if bChanged then Just $ Application a b' else Nothing where
	b' = map (uncurry fromMaybe) bb
	bChanged = any (isJust . snd) bb

processAtom err a f = case lookupFact a f of
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"
	Just Bot -> error $ err ++ ".rewriteExitL.Bot"
	Just (PElem (LetNode args body)) -> Just (args, body)
	_ -> Nothing

processAtom2 err (Atom a) = processAtom err a

process2 :: (ExpressionFunctor ExpressionFix -> Maybe ExpressionFix) -> Rewrite ExpressionFix
process2 f = para ff where
	ff x = let x' = uncurry fromMaybe <$> x
		in case f x' of
			Nothing -> if F.any (isJust . snd) x then Just (embed x') else Nothing
			x -> x

phi2 :: FactBase ListFact -> ExpressionFunctor ExpressionFix -> Maybe ExpressionFix
phi2 _ (ConstantF _) = Nothing
phi2 f a @ (AtomF aa) = do
	([], e) <- processAtom "rewriteExpression" aa $ xtrace ("factBase-atom {" ++ show a ++ "}") f
	return e
phi2 f (ApplicationF (Atom a) b') = case processAtom "rewriteApplication.Single" a f of
	Nothing -> Nothing
	Just ([], expr) -> Just $ Application expr b'
	Just (args, expr) -> inlineApplication args b' f expr
-- phi2 f (Application a bb) | isAtomApplication (a) = rewriteApplication a bb f
-- phi2 f (Application a bb) = Nothing
