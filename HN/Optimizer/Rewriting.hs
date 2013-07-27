{-# LANGUAGE GADTs #-}
module HN.Optimizer.Rewriting (rewriteExpression, ListFact) where

import Data.Maybe
import Control.Applicative
import Compiler.Hoopl
import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import Utils

type Rewrite a = a -> Maybe a

composeR :: Rewrite a -> Rewrite a -> Rewrite a
composeR a b x = if ch then Just result else Nothing where
	result = dropR b $ dropR a x
	ch = changed (a x) || changed (b x)

type ListFact = WithTopAndBot DefinitionNode	

changed Nothing = False
changed _ = True
	
dropR :: Rewrite a -> a -> a
dropR a x = fromMaybe x (a x)
	
unitR :: Rewrite a
unitR = const Nothing
	
liftR :: (t -> a) -> Rewrite t -> t -> a
liftR f rf x = f $ dropR rf x

apply1 :: (a -> b) -> Rewrite a -> a -> Maybe b
apply1 cons rewriter el = fmap cons $ rewriter el

apply2 :: (h -> t -> l) -> Rewrite h -> Rewrite t -> h -> t -> Rewrite l
apply2 cons rh rt h t = undefined  

uncondLookupFact err a f = case lookupFact a f of
	Just x -> x
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"

rewriteApplication (Atom a) b f = case processAtom "rewriteApplication.Single" a f of
	Nothing -> Application (Atom a) <$> rewriteArgs f b
	Just ([], expr) -> Application expr <$> Just (dropR (rewriteArgs f) b)
	Just (args, expr) -> inlineApplication args b f expr	

rewriteApplication (Application (Atom a) b) c f = case processAtom "rewriteApplication.Double.1" a f of
	Nothing -> Nothing
	Just ([], _) -> error "rewriteApplication.double.LetNode.var"
	Just (outerParams, body) -> case body of
 		Atom aOuterBody -> case processAtom "rewriteApplication.Double.2" aOuterBody f of
			Just (innerParams, innerBody) -> case inlineApplication innerParams c f innerBody of
				Just (Application aa bb) -> (\x -> Application x bb) <$> (Just $ dropR (inlineApplication outerParams b f) aa)
				Just _ -> error "rewriteApplication.double.LetNode.fn.Just.noApp"
				Nothing -> Nothing						
			_ -> error "rewriteApplication.double.LetNode.fn.atombody.cannotInline"
 		_ -> error "rewriteApplication.double.LetNode.fn"

rewriteApplication a b f = case rewriteExpression f a of
	Nothing -> Application a <$> rewriteArgs f b 
	Just _ -> error "rapp.Just" 

inlineApplication formalArgs actualArgs f
	= Just . dropR (rewriteExpression (flip mapUnion f $ mapFromList $ zip formalArgs $ map (PElem . LetNode []) actualArgs)) 

rewriteArgs  :: FactBase ListFact -> Rewrite [Expression Label]
rewriteArgs f [] = Nothing 
rewriteArgs f (h : t) = lift2 (:) (rewriteExpression f) h (rewriteArgs f) t

lift2 :: (t -> a1 -> a) -> Rewrite t -> t -> Rewrite a1 -> a1 -> Maybe a 
lift2 cons rewriteHead h rewriteTail t = case rewriteHead h of
	Nothing -> cons h <$> rewriteTail t
	Just h' -> cons h' <$> Just (dropR rewriteTail t)

rewriteExpression :: FactBase ListFact -> Rewrite (Expression Label)
rewriteExpression f = fmap (dropR $ rewriteExpression f) . rewriteExpression2 f

rewriteExpression2 :: FactBase ListFact -> Rewrite (Expression Label)
rewriteExpression2 f expr =  case expr of
	Constant _ -> Nothing
	Atom a -> case processAtom "rewriteExpression2" a f of
		Just ([], e) -> Just e
		_ -> Nothing
	Application a b -> xtrace ("rewriteExpression.rewriteApplication of " ++ show a ++ " to " ++ show b) $ rewriteApplication a b f
	
processAtom err a f = case uncondLookupFact err a f of
 	Bot -> error "rewriteExitL.Bot"
 	PElem (LetNode args body) -> Just (args, body)
	_ -> Nothing
