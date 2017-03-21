{-# LANGUAGE Rank2Types #-}
module HN.Optimizer.Pass (runPass, pureFRewrite, pureBRewrite, defaultFactsHack, PassParams(..), runPassF, runPassB) where
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Node
import Compiler.Hoopl

pureBRewrite :: FuelMonad m => (forall e x . n e x -> Fact x f -> Maybe (Graph n e x)) -> BwdRewrite m n f
pureBRewrite ff = mkBRewrite $ \a b -> return $ ff a b

pureFRewrite :: FuelMonad m => (forall e x . n e x -> f -> Maybe (Graph n e x)) -> FwdRewrite m n f
pureFRewrite ff = mkFRewrite $ \a b -> return $ ff a b

defaultFactsHack l n = zip (successors n) $ repeat $ fact_bot l

runPass f makeFacts (graph, facts, _) = f (JustC [firstLabel]) graph $ makeFacts facts firstLabel where
	firstLabel = runSimpleUniqueMonad freshLabel

type PassParamsF t f = PassParams (FactBase t -> Label -> FactBase f) (FwdTransfer Node f) (FwdRewrite SimpleFuelMonad Node f)

runPassF :: Lattice f => PassParamsF t f -> Pass t f
runPassF (PassParams makeFacts tf rf) = runPass (analyzeAndRewriteFwd passF) makeFacts where
	passF = FwdPass
		{ fp_lattice = dataflowLattice
		, fp_transfer = tf
		, fp_rewrite = rf
		}

runPassB (PassParams makeFacts tf rf) = runPass (analyzeAndRewriteBwd passB) makeFacts where
	passB = BwdPass
		{ bp_lattice = dataflowLattice
		, bp_transfer = tf
		, bp_rewrite = rf
		}

data PassParams a b c = PassParams
	{ ppConvertFacts :: a
	, ppTransfer :: b
	, ppRewrite :: c
	}
