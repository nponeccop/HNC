{-# LANGUAGE GADTs #-}
module HN.Optimizer.Visualise where

import HN.Optimizer.Node
import Data.Functor.Fixedpoint

instance Show (Node e x) where
	show (Entry l) = show l
	show (Exit dn) = show dn

instance Show DefinitionNode where
	show x = case x of
		LetNode l e -> case l of
		 	[] -> " = " ++ show e
			_ -> " " ++ concatMap (\l -> show l ++ " ") l ++ "= " ++ show e
		ArgNode -> " :: @"
		LibNode -> " :: #"

instance Show a => Show (ExpressionFunctor a) where
	show e = case e of
		Constant c -> show c
		Atom aa -> show aa
		Application a b -> show a ++ concatMap (\b -> ' ' : show b) b