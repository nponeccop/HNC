{-# LANGUAGE GADTs #-}
module HN.Optimizer.Visualise where

import HN.Optimizer.Node
import Compiler.Hoopl

instance Show (Node e x) where
	show (Entry l) = show l
	show (Exit dn) = show dn

foo :: Graph Node C C -> String
foo = showGraph show

instance Show DefinitionNode where
	show x = case x of
		LetNode l e -> case l of
		 	[] -> " = " ++ show e
			_ -> " " ++ concatMap (\l -> show l ++ " ") l ++ "= " ++ show e
		ArgNode -> " :: @"
		LibNode -> " :: #"
