{-# LANGUAGE DeriveTraversable #-}
module Unifier.Unifier (T(..), UTerm(..), unify, subsumes) where
import Control.Unification

data T a 
	= T String 
	| TT [a] 
	| TD String [a]
	deriving (Eq, Show, Functor, Traversable, Foldable)

instance Unifiable T where
	zipMatch (TT a1) (TT a2) = TT <$> maybeZip a1 a2
	zipMatch (T a) (T b) | a == b = Just $ T a
	zipMatch (TD l1 a1) (TD l2 a2) | l1 == l2 = TD l1 <$> maybeZip a1 a2
	zipMatch _ _ = Nothing

maybeZip a b | length a == length b = Just $ zipWith (curry Right) a b
maybeZip _ _ = Nothing
