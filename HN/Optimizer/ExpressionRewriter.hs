{-# LANGUAGE FlexibleContexts #-}
module HN.Optimizer.ExpressionRewriter where
import Control.Monad
import Data.Functor.Foldable
import Data.Maybe

process rewrite = para phi where
	phi cons = let
			foo = embed $ uncurry fromMaybe <$> cons
			bar = rewrite foo
		in
			if isJust bar || any (isJust . snd) cons 
				then mplus bar $ Just foo 
				else Nothing	
