{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Utils.Kmett (unzippedPara, mapValues) where

import Control.Newtype
import Data.Bifunctor
import Data.Bifunctor.Tannen
import Data.Functor.Adjunction
import Data.Functor.Foldable

unzippedPara f = para $ uncurry f . unzipR

instance Newtype (Tannen f p a b) (f (p a b)) where
	pack = Tannen
	unpack = runTannen	

mapValues x = under Tannen $ second x

