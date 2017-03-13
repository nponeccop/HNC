{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, NoMonomorphismRestriction, TypeFamilies #-}

module HN.TypeTools (isFunctionType, hasFunctionalType, cppCannotInferReturnType, typeTu, typeTv, mapTypeTV, addTU, tv) where

import SPL.Types
import qualified Data.Set as S
import Data.Foldable
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Maybe

makeBaseFunctor ''T

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

collectSet vp = tCata $ \case
       (vp -> Just v) -> S.singleton v
       x -> S.unions $ toList x

matchTTU = \case
       TUF a -> Just a
       _ -> Nothing

matchTTV = \case
       TVF a -> Just a
       _ -> Nothing

mapTypeTV f = tCata $ \case
       TVF a -> f a
       t -> embed t
