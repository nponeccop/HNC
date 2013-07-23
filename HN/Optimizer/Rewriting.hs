{-# LANGUAGE DeriveFunctor #-}

import Data.Functor.Fixedpoint

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
-}	


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





{- case rewriteTop t of
	Nothing -> t
 	Just t' -> rewriteTop t'
-}	
{-
simpleDeepRewrite simpleRewriteTop t = simpleRewriteTop $ case t of
	Atom -> Atom
	Application a b -> Application $ (simpleRewriteTop a) (simpleRewriteTop b) 
-}





























