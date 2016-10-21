{-# LANGUAGE FlexibleContexts #-}
module HN.Optimizer.ExpressionRewriter (Rewrite, ChildRewrites(..), rewrite) where
import Control.Applicative
import Data.Functor.Foldable
import Data.Maybe

type Rewrite a = a -> Maybe a

data ChildRewrites = WithChildren | WithoutChildren

handleChildren flag cons = embed $ f flag <$> cons where
	f WithChildren = uncurry fromMaybe
	f WithoutChildren = fst

rewrite flag rewriteFn = para $ \cons -> rewriteFn (handleChildren flag cons) <|> liftChildRewrites cons

hasChildRewrites cons = any (isJust . snd) cons

liftChildRewrites cons = if hasChildRewrites cons
	then Just $ handleChildren WithChildren cons
	else Nothing

deep process a = xdeep <$> process a where
	xdeep a = maybe a xdeep $ process a

composeRewrites :: Rewrite a -> Rewrite a -> Rewrite a
composeRewrites f g x = maybe (f x) (Just . dropR f) $ g x

dropR :: Rewrite a -> a -> a
dropR a x = fromMaybe x (a x)
