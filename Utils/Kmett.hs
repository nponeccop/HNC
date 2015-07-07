{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Utils.Kmett (unzippedPara, mapValues) where

import Control.Newtype
import Data.Bifunctor
import Data.Bifunctor.Tannen
import Data.Functor.Foldable

unzippedPara f = para $ \a -> f (fmap fst a) (fmap snd a)

instance Newtype (Tannen f p a b) (f (p a b)) where
	pack = Tannen
	unpack = runTannen	

mapValues x = under Tannen $ second x

