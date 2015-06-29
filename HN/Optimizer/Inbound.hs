{-# LANGUAGE GADTs #-}

module HN.Optimizer.Inbound (runF) where
import Compiler.Hoopl
import HN.Optimizer.Node
import HN.Optimizer.Pass

-- FORWARD pass
--
-- Type and definition of the lattice
type IntFact = Int

transferF :: Node e x -> IntFact -> Fact x IntFact
transferF (Entry _) _ = 0
transferF n @ (Exit _) _ = distributeXfer intLattice (\_ _ -> 1) n undefined

passF = FwdPass
	{ fp_lattice = intLattice
	, fp_transfer = mkFTransfer transferF
	, fp_rewrite = noFwdRewrite }

runF :: Pass any IntFact
runF = runPass (analyzeAndRewriteFwd passF) $ \_ _ -> noFacts


intLattice = DataflowLattice
	{ fact_name = "IntFact"
	, fact_bot = 0
	, fact_join = \ _ (OldFact old) (NewFact new) ->
		if new == 0
			then (NoChange, old)
			else (SomeChange, new + old)
	}
