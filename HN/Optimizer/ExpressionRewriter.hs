{-# LANGUAGE FlexibleContexts #-}
module HN.Optimizer.ExpressionRewriter (process, process', Rewrite) where
import Control.Applicative
import Data.Functor.Foldable
import Data.Maybe

type Rewrite a = a -> Maybe a

process rewrite = para phi where
	phi cons = rewrite (applyChildRewrites cons) <|> liftChildRewrites cons

process' rewrite = para phi where
  phi cons = rewrite (ignoreChildRewrites cons) <|> liftChildRewrites cons

applyChildRewrites cons = embed $ uncurry fromMaybe <$> cons

ignoreChildRewrites cons = embed $ fst <$> cons

hasChildRewrites cons = any (isJust . snd) cons

liftChildRewrites cons = if hasChildRewrites cons
	then Just $ applyChildRewrites cons
	else Nothing

deep process a = xdeep <$> process a where
	xdeep a = maybe a xdeep $ process a

composeRewrites :: Rewrite a -> Rewrite a -> Rewrite a
composeRewrites f g x = maybe (f x) (Just . dropR f) $ g x

dropR :: Rewrite a -> a -> a
dropR a x = fromMaybe x (a x)
