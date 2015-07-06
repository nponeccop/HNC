{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module HN.Optimizer.ClassyLattice where
import Compiler.Hoopl
import Data.Maybe
import qualified Data.Map as M

class Lattice a where
	join :: OldFact a -> NewFact a -> Maybe a
	join a b= case fact_join dataflowLattice (error "No label") a b of
		(SomeChange, n) -> Just n
		_ -> Nothing
		
	bot :: a
	bot = fact_bot dataflowLattice
	dataflowLattice :: DataflowLattice a
	
	dataflowLattice = DataflowLattice 
		{ fact_name = "hooplLattice"
		, fact_bot = bot
		, fact_join = \_ o @ (OldFact oo) n -> maybe (NoChange, oo) ((,) SomeChange) $ join o n 
		} 

instance Lattice a => Monoid a where
	mappend a b = fromMaybe a $ join (OldFact a) (NewFact b)
	mempty = bot

instance (Lattice v, Ord k) => Lattice (M.Map k v) where
	bot = M.empty
	join = fromJoinFun $ joinMaps $ fact_join dataflowLattice

fromJoinFun :: JoinFun a -> OldFact a -> NewFact a -> Maybe a
fromJoinFun jf a b = case jf (error "No label") a b of
	(SomeChange, n) -> Just n
	_ -> Nothing

mereJoin a b = fromMaybe a $ join (OldFact a) (NewFact b)

instance (Lattice a, Lattice b) => Lattice (a, b) where
	dataflowLattice = pairLattice dataflowLattice dataflowLattice
