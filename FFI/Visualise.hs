module FFI.Visualise where

import Data.Functor.Foldable

import HN.TypeTools
import SPL.Types
import Utils
import qualified Data.Map as M

showType :: T -> String
showType t = cata f t where
	f (TF x) = x
	f (TTF x) = inParens $ concatMap (++ " ") (init x) ++ "-> " ++ last x
	f (TDF a b) = a ++ inAngular (joinStr " " b)
	f (TVF x) = "?" ++ x
	f (TUF x) = "??" ++ x
	_ = show t

showAsFFI typeEnv = concatMap f $ M.toList typeEnv where
	f (name, t) = name ++ " = " ++ showType t ++ "\n"
