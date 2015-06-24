{-# LANGUAGE FlexibleContexts #-}
module HN.Optimizer.ExpressionRewriter where
import Control.Monad
import Data.Functor.Foldable
import Data.Maybe

type Rewrite a = a -> Maybe a

process rewrite = para phi where
	phi cons = let
			foo = embed $ uncurry fromMaybe <$> cons
			bar = rewrite foo
		in
			if isJust bar || any (isJust . snd) cons
				then mplus bar $ Just foo
				else Nothing

process' rewrite = para phi where
	phi cons = let
			foo = embed $ uncurry fromMaybe <$> cons
			bar = rewrite $ embed $ fst <$> cons
		in
			if isJust bar || any (isJust . snd) cons
				then mplus bar $ Just foo
				else Nothing

deep process a = xdeep <$> process a where
	xdeep a = maybe a xdeep $ process a

composeRewrites :: Rewrite a -> Rewrite a -> Rewrite a
composeRewrites f g x = maybe (f x) (Just . dropR f) $ g x

dropR :: Rewrite a -> a -> a
dropR a x = fromMaybe x (a x)

applyChildRewrites xx = if any (isJust . snd) xx
	then Just $ embed $ uncurry fromMaybe <$> xx
	else Nothing
