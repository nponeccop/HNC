{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables #-}
module HN.Optimizer.Lattice where

import Compiler.Hoopl
import Data.Void
import Safe.Exact

liftedLattice ff name = addPoints' name f where
	f _ o @ (OldFact oo) n = case ff o n of
		Bot -> (NoChange, PElem oo)
		x -> (SomeChange, x)

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

listLattice :: JoinFun a -> UnnamedLattice [a]
listLattice = flip $ \name -> addPoints' name . joinLists

joinLists
  :: (t -> OldFact a -> NewFact a1 -> (ChangeFlag, a))
     -> t
     -> OldFact [a]
     -> NewFact [a1]
     -> (ChangeFlag, Pointed C b [a])

joinLists _ _ (OldFact o) (NewFact []) = (NoChange, PElem o)

joinLists joinElems label (OldFact o) (NewFact n) = case j of
		Nothing -> (SomeChange, Top)
		Just j -> if any (isSomeChange . fst) j
			then (SomeChange, PElem $ map snd j)
			else (NoChange, PElem o)
	where
		j = zipWithExactMay (joinElems label) (map OldFact o) (map NewFact n)
		isSomeChange SomeChange = True
		isSomeChange NoChange = False
