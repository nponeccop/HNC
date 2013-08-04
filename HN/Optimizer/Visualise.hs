{-# LANGUAGE GADTs #-}
module HN.Optimizer.Visualise where
import Compiler.Hoopl
import Data.Functor.Foldable
import HN.Optimizer.Node
import Utils

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

foo x = concatMap ff $ mapToList x where
	ff (l, x) = show l ++ " => " ++ case x of
		
		Top -> "T\n"
		Bot -> "⊥\n"
		PElem x -> concatMap bar x ++ "\n"
		
bar x = case x of
		Top -> "T\n"
		Bot -> "⊥\n"
		PElem x -> cata phi x ++ " "
		
phi x = case x of
	Atom x -> show x
	Constant x -> show x
	Application a b -> "(" ++ a ++ " " ++ joinStr " " b ++ ")"

