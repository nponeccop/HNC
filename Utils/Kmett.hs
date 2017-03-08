{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Utils.Kmett (unzippedPara, mapValues) where

import Control.Lens.Iso
import Control.Lens.TH
import Control.Lens.Wrapped
import Data.Bifunctor
import Data.Bifunctor.Tannen
import Data.Functor.Adjunction
import Data.Functor.Foldable

unzippedPara f = para $ uncurry f . unzipR

mapValues x = under (_Wrapping Tannen) $ second x

makeWrapped ''Tannen


