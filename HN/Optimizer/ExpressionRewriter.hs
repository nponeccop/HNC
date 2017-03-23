{-# LANGUAGE FlexibleContexts #-}

module HN.Optimizer.ExpressionRewriter (Rewrite, ChildRewrites(..), rewrite, rewriteNode) where

import Control.Applicative
import Data.Functor.Foldable
import Data.Maybe
import Compiler.Hoopl

import HN.Optimizer.Node


type Rewrite a = a -> Maybe a

data ChildRewrites = WithChildren | WithoutChildren

handleChildren WithChildren = embed . fmap (uncurry fromMaybe)
handleChildren WithoutChildren = embed . fmap fst

rewrite flag rewriteFn = para $ liftA2 (<|>) (rewriteFn . handleChildren flag) liftChildRewrites

rewriteNode :: DefinitionNode
	        -> ChildRewrites
	        -> (FactBase a -> Rewrite ExpressionFix)
	        -> FactBase a
	        -> Maybe DefinitionNode
rewriteNode (LetNode l expr) c r f = LetNode l <$> rewrite c (r f) expr
rewriteNode _ _ _ _ = Nothing

liftChildRewrites cons | any (isJust . snd) cons = Just $ handleChildren WithChildren cons
liftChildRewrites _ = Nothing

deep process a = xdeep <$> process a where
	xdeep a = maybe a xdeep $ process a

composeRewrites :: Rewrite a -> Rewrite a -> Rewrite a
composeRewrites f g x = maybe (f x) (Just . dropR f) $ g x

dropR :: Rewrite a -> a -> a
dropR a x = fromMaybe x (a x)
