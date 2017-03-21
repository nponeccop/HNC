module HN.Optimizer.Frontend (optimizeHN, withGraph, newInliner, noInliner, oldInliner, ootrace) where
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler
import HN.Optimizer.Inbound (runF)
import HN.Optimizer.Inliner2 (runB)
import qualified HN.Optimizer.FormalArgumentsDeleter as AAD
import qualified HN.Optimizer.ArgumentValues as AV
import qualified HN.Optimizer.ArgumentDeleter as AD
import qualified HN.Optimizer.Arity as Ar
import qualified HN.Optimizer.SimpleAlias as SA

import HN.Optimizer.Node (Pass)
import Utils
import Debug.Trace
import HN.Optimizer.Visualise

optimizeHN libraryTypes = map (withGraph libraryTypes (\whileLabel -> fromTuple . runSimpleUniqueMonad . runWithFuel infiniteFuel . passes whileLabel . toTuple))

oldInliner whileLabel = runF >=> runB whileLabel

newInliner = Ar.runB >=> AV.runAv >=> AD.runF >=> AAD.runB

passes whileLabel = newInliner >=> SA.runB >=> newInliner >=> SA.runB >=> newInliner >=> oldInliner whileLabel

noInliner :: Pass any ()
noInliner (x, _, _) = return (x, noFacts, undefined)

otrace _ x = x
ootrace x = trace x

toTuple agraph = (agraph, undefined, undefined)
fromTuple (agraph, facts, _) = otrace (formatGraph agraph ++ show facts) agraph

withGraph libNames f def = decompileGraph (xtrace "labelNames" labelNames) $ f whileLabel graph where
	(graph, (nameLabels, labelNames)) = compileGraph libNames def
	whileLabel = tracedUncondLookup "Frontend.whileF" "whileF" nameLabels
