{-# LANGUAGE GADTs #-}
module HN.Optimizer.Arity (runB) where

import Compiler.Hoopl
import qualified Data.Map as M

import HN.Optimizer.ArgumentValues
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Node
import HN.Optimizer.Pass

transferB' :: Node e x -> Fact x ArgFact -> ArgFact
transferB' (Entry l) o @ (curFact, factBase) = newFact where
	baseFact = M.lookup l factBase
	newFact = case baseFact of
		Nothing -> (curFact, M.insert l curFact factBase)
		Just baseFact -> case join (OldFact baseFact) (NewFact curFact) of
			Nothing -> (baseFact, factBase)
			Just newFact -> (newFact, M.insert l newFact factBase)

transferB' (Exit d) o = transferB d o

transferB :: DefinitionNode -> FactBase ArgFact -> ArgFact
transferB n _ = ((callFact, bot), bot) where
	callFact = case n of
		ArgNode -> PElem []
		LibNode -> Top
		LetNode args _ -> PElem $ map (const bot) args

passB = BwdPass
	{ bp_lattice = dataflowLattice
	, bp_transfer = mkBTransfer transferB'
	, bp_rewrite = noBwdRewrite
	}

runB :: Pass any ArgFact
runB = runPass (analyzeAndRewriteBwd passB) (\_ _ -> noFacts)
