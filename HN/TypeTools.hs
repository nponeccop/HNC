{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module HN.TypeTools (isFunctionType, hasFunctionalType, cppCannotInferReturnType, typeTu, typeTv, mapTypeTV, addTU, tv) where

import SPL.Types
import qualified Data.Set as S
import qualified Data.Foldable as F
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Functor.Foldable
import Data.Monoid

newtype Union a = Union (S.Set a)

instance Ord a => Monoid (Union a) where
   mappend (Union a) (Union b) = Union $ S.union a b
   mempty = Union S.empty

isFunctionType (TT _) = True
isFunctionType _ = False

hasFunctionalType = isJust . find isFunctionType . init

cppCannotInferReturnType x = not . S.null $ typeTu (last x) S.\\ typeTu (TT $ init x)

tv x = TV $ 't' : show x

foldMapCata :: (Recursive t, Monoid a, Foldable (Base t)) => (Base t a -> a) -> t -> a
foldMapCata = cata . flip (liftA2 (<>)) F.fold

collectSet :: (Recursive t, Foldable (Base t), Ord a) => (Base t (S.Set a) -> Maybe a) -> t -> S.Set a
collectSet vp (foldMapCata (maybe mempty S.singleton . vp) -> a) = a

matchTTU :: TF t -> Maybe String
matchTTU = \case (TUF a) -> Just a; _ -> Nothing

matchTTV :: TF t -> Maybe String
matchTTV = \case (TVF a) -> Just a; _ -> Nothing

typeTu :: T -> S.Set String
typeTu = collectSet matchTTU

typeTv :: T -> S.Set String
typeTv = collectSet matchTTV

mapTypeTV :: (String -> T) -> T -> T
mapTypeTV f = cata $ \case TVF a -> f a; t -> embed t

addTU s = mapTypeTV f where
	f x | x `S.member` s = TU x
	f x = TV x
