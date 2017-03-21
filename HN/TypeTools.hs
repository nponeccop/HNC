{-# LANGUAGE FlexibleContexts, LambdaCase, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, NoMonomorphismRestriction, TypeFamilies, StandaloneDeriving #-}

module HN.TypeTools (isFunctionType, hasFunctionalType, cppCannotInferReturnType, typeTu, typeTv, mapTypeTV, addTU, tv, TF(..)) where

import SPL.Types
import qualified Data.Set as S
import Data.Foldable
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Maybe
import Utils.Kmett (foldMapCata)

makeBaseFunctor ''T

deriving instance Show a => Show (TF a)

isFunctionType (TT _) = True
isFunctionType _ = False

hasFunctionalType = isJust . find isFunctionType . init

cppCannotInferReturnType x = not . S.null $ typeTu (last x) S.\\ typeTu (TT $ init x)

tv x = TV $ 't' : show x

typeTu = collectSet matchTTU

typeTv = collectSet matchTTV

addTU s = mapTypeTV f where
	f x | x `S.member` s = TU x
	f x = TV x

tCata :: (TF a -> a) -> T -> a
tCata = cata

collectSet vp = foldMapCata (maybe S.empty S.singleton . vp)

matchTTU = \case
	TUF a -> Just a
	_ -> Nothing

matchTTV = \case
	TVF a -> Just a
	_ -> Nothing

mapTypeTV f = tCata $ \case
	TVF a -> f a
	t -> embed t
