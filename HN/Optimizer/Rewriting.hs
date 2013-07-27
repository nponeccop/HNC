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

rewriteApplication (Atom a) b f = let onlyArgs = Application (Atom a) <$> rewriteArgs f b
	in case lookupFact a f of
		Nothing -> error "rapp.Atom.Nothing"
		Just x -> case processAtom2 x of
 			Nothing -> onlyArgs
			Just ([], expr) -> Application expr <$> Just (dropR (rewriteArgs f) b)
			Just (args, expr) -> inlineApplication args b f expr	

rewriteApplication (Application (Atom a) b) c f = case lookupFact a f of
	Nothing -> error "rewriteApplication.double.Nothing"
	Just x -> case x of
		Top -> error "rewriteApplication.double.Top"
		Bot -> error "rewriteApplication.double.Bot"
		PElem x -> case x of
 			LetNode [] _ -> error "rewriteApplication.double.LetNode.var"
 			LetNode outerParams body -> case body of
 				Atom aOuterBody -> case lookupFact aOuterBody f of
					Just (PElem (LetNode innerParams innerBody)) -> case inlineApplication innerParams c f innerBody of
						Just (Application aa bb) -> (\x -> Application x bb) <$> (Just $ dropR (inlineApplication outerParams b f) aa)
						Just _ -> error "rewriteApplication.double.LetNode.fn.Just.noApp"
						Nothing -> Nothing						
					_ -> error "rewriteApplication.double.LetNode.fn.atombody.cannotInline"
 				_ -> error "rewriteApplication.double.LetNode.fn"
			LibNode -> error "rewriteApplication.double.LibNode"
			ArgNode -> Nothing -- error "rewriteApplication.double.ArgNode"

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

rewriteExpression f = fmap (dropR $ rewriteExpression f) . rewriteExpression2 f

rewriteExpression2 :: FactBase ListFact -> Rewrite (Expression Label)
rewriteExpression2 f expr =  case expr of
	Constant _ -> Nothing
	Atom a -> case lookupFact a f of
		Nothing -> error "rewriteExpression.Nothing"
		Just x -> xtrace ("rewriteExpression Atom " ++ show a ++ "[" ++ show x ++ "]" ++ show f) $ processAtom x
	Application a b -> xtrace ("rewriteExpression.rewriteApplication of " ++ show a ++ " to " ++ show b) $ rewriteApplication a b f

processAtom :: ListFact -> Maybe (Expression Label)
processAtom x = case processAtom2 x of
	Just ([], e) -> Just e
	_ -> Nothing

processAtom2 :: ListFact -> Maybe ([Label], Expression Label)
processAtom2 x = case x of
 	Top -> Nothing
 	Bot -> error "rewriteExitL.Bot"
 	PElem e -> case e of
		ArgNode -> Nothing -- error "processFact.ArgNode"
		LetNode args body -> Just (args, body)
		LibNode -> Nothing
