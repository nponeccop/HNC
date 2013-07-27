module HN.Optimizer.Rewriting where

import Data.Maybe

type Rewrite a = a -> Maybe a

composeR :: Rewrite a -> Rewrite a -> Rewrite a
composeR a b x = if ch then Just result else Nothing where
	result = dropR b $ dropR a x
	ch = changed (a x) || changed (b x)
	

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

{-
x = cata phi where
	phi Atom = Nothing
	phi (Application a b) = apply Application a b

apply2 cons a b = case (a, b) of
	(Nothing, Nothing) -> Nothing
 	(Just a', Nothing) -> Just $ cons a' b
  	(Nothing, Just b') -> Just $ cons a b' 



rewrite t = case rewriter t of
	Nothing -> rewriteChildren t
	x @ (Just t') -> case rewriteChildren t' of
		Nothing -> x
		xx -> xx

rewrite2 = compose rewriter rewriteChildren

compose f g t = case f t of
	Nothing -> g t
	x @ (Just t') -> case g t' of
		Nothing -> x
		xx -> xx

apply f t jt' = case jt' of
	Nothing -> f 

data GTerm a = Atom | Application (GTerm a) (GTerm a) deriving (Functor)

type Term = Fix GTerm

rewriteTop :: Term -> Maybe Term
rewriteTop x = undefined

rewriteChildren :: Term -> Maybe Term
rewriteChildren x = undefined

simpleRewriteTop :: (Term -> Maybe Term) -> Term -> Term
simpleRewriteTop rewriteTop t = case rewriteTop t of
	Nothing -> t
 	Just t' -> simpleRewriteTop rewriteTop t'


newtype Rewritten a = Rewritten (Maybe a)
-}	

{- case rewriteTop t of
	Nothing -> t
 	Just t' -> rewriteTop t'
-}	
{-
simpleDeepRewrite simpleRewriteTop t = simpleRewriteTop $ case t of
	Atom -> Atom
	Application a b -> Application $ (simpleRewriteTop a) (simpleRewriteTop b) 
-}





























