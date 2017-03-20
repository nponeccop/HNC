{-# LANGUAGE LambdaCase #-}
module FFI.Visualise (showType) where

import Data.Functor.Foldable

import HN.TypeTools
import SPL.Types
import Utils

showType (TT y) = showTT $ showTypeP <$> y
showType t = showTypeP t

showTT x = concatMap (++ " ") (init x) ++ "-> " ++ last x

showTypeP :: T -> String
showTypeP = cata $ \case
	TF x -> x
	TTF x -> inParens $ showTT x
	TDF a b -> a ++ inAngular (joinStr " " b)
	TVF x -> "?" ++ x
	TUF x -> "??" ++ x
	t -> show t