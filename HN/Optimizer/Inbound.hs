{-# LANGUAGE GADTs #-}

module HN.Optimizer.Inbound (runF) where
import Compiler.Hoopl
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.Utils

type IntFact = Int

transferF :: DefinitionNode -> IntFact -> FactBase IntFact
transferF dn _ = distributeXfer dataflowLattice (\_ _ -> 1) (Exit dn) undefined

runF :: Pass any IntFact
runF = runPassF PassParams
	{ ppConvertFacts = \_ _ -> noFacts
	, ppTransfer = mkFTransfer $ transferExitF transferF
	, ppRewrite = noFwdRewrite
	}

instance Lattice Int where
	bot = 0
	join (OldFact _) (NewFact 0) = Nothing
	join (OldFact old) (NewFact new) = Just $ new + old
