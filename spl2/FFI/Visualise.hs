module FFI.Visualise where

import SPL.Types
import Utils
import qualified Data.Map as M

showType t = case t of
	T x -> x
	TT x -> concatMap (\x -> showTypeP x ++ " ") (init x) ++ "-> " ++ showType (last x)
	TD a b -> a ++ inAngular (joinStr " " (map showType b))
	TU a ->  "?" ++ a
	TV a -> "??" ++ a
	_ -> show t

showTypeP x = case x of
	TT _ -> inParens (showType x)
	_ -> showType x

showAsFFI typeEnv = concatMap f $ M.toList typeEnv where
	f (name, t) = name ++ " = " ++ showType t ++ "\n"