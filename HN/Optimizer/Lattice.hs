{-# LANGUAGE GADTs #-}
module HN.Optimizer.Lattice where

import Compiler.Hoopl
import Data.Void

liftedLattice ff name = addPoints' name f where
	f _ o @ (OldFact oo) n = case ff o n of
		Bot -> (NoChange, PElem oo)
		PElem a -> (SomeChange, PElem a)
		Top -> (SomeChange, Top)

plusLattice :: (Num a, Eq a) => DataflowLattice a
plusLattice = eqBotLattice "IntFact" 0 (+)

eqBotLattice name b j = DataflowLattice 
	{ fact_name = name
	, fact_bot = b
	, fact_join = const $ \(OldFact old) (NewFact new) ->
		if new == b 
			then (NoChange, old)
			else (SomeChange, j old new)
	}
	
type UnnamedLattice a = String -> DataflowLattice (WithTopAndBot a)

binaryLattice :: UnnamedLattice Void
binaryLattice name = addPoints name $ const $ \(OldFact x) -> absurd x

flatEqLattice :: Eq a => UnnamedLattice a
flatEqLattice = liftedLattice $ \(OldFact o) (NewFact n) -> 
	if o == n then Bot else Top

notTop Top = False
notTop _ = True

listLattice x = liftedLattice ff where
	ff (OldFact o) (NewFact n) = let j = zipWith (fact_join x undefined) (map OldFact o) (map NewFact n)
		in if any (notTop . snd) j 
			then PElem $ map snd j 
			else Top 
