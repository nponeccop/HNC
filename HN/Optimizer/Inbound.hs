{-# LANGUAGE GADTs #-}

module HN.Optimizer.Inbound (runF) where
import Compiler.Hoopl
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.Utils

-- FORWARD pass
--
-- Type and definition of the lattice
type IntFact = Int

transferF :: DefinitionNode -> IntFact -> FactBase IntFact
transferF dn _ = distributeXfer intLattice (\_ _ -> 1) (Exit dn) undefined

passF = FwdPass
	{ fp_lattice = intLattice
	, fp_transfer = mkFTransfer $ transferExitF transferF
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
