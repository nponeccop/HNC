{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
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

isFunctionType :: T -> Bool
isFunctionType (TT _) = True
isFunctionType _ = False

hasFunctionalType :: [T] -> Bool
hasFunctionalType = isJust . find isFunctionType . init

cppCannotInferReturnType :: [T] -> Bool
cppCannotInferReturnType x = not . S.null $ typeTu (last x) S.\\ typeTu (TT $ init x)

tv :: Show a => a -> T
tv x = TV $ 't' : show x

foldMapCata :: (Recursive t, Monoid a, Foldable (Base t)) => (Base t a -> a) -> t -> a
foldMapCata = cata . flip (liftA2 (<>)) F.fold

collectSet :: (Foldable (Base t), Ord a, Recursive t) => (Base t (Union a) -> Maybe a) -> t -> S.Set a
collectSet vp (foldMapCata (Union . maybe mempty S.singleton . vp) -> Union a) = a

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

addTU :: S.Set String -> T -> T
addTU s = mapTypeTV f where
  f x | x `S.member` s = TU x
  f x = TV x
