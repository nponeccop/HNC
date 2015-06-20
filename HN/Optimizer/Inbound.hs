{-# LANGUAGE GADTs #-}

module HN.Optimizer.Inbound (runF) where
import Compiler.Hoopl
import HN.Optimizer.Node
import HN.Optimizer.Pass

-- FORWARD pass
--
-- Type and definition of the lattice
type IntFact = Int

transferF = mkFTransfer ft where
	ft :: Node e x -> IntFact -> Fact x IntFact
	ft (Entry _) _ = 0
	ft n @ (Exit _) _ = distributeXfer intLattice (\_ _ -> 1) n undefined

passF = FwdPass
	{ fp_lattice = intLattice
	, fp_transfer = transferF
	, fp_rewrite = noFwdRewrite }

runF
  :: (Graph Node C C, t1, t2)
     -> SimpleFuelMonad
          (Graph Node C C, FactBase IntFact, MaybeO C IntFact)
runF = runPass (analyzeAndRewriteFwd passF) $ \_ _ -> mapEmpty


intLattice = DataflowLattice
	{ fact_name = "IntFact"
	, fact_bot = 0
	, fact_join = \ _ (OldFact old) (NewFact new) ->
		if new == 0
			then (NoChange, old)
			else (SomeChange, new + old)
	}
