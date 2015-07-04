module HN.Optimizer.Frontend (optimizeHN, withGraph) where
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler
import HN.Optimizer.Inbound (runF)
import HN.Optimizer.Inliner2 (runB)
import qualified HN.Optimizer.FormalArgumentsDeleter as AAD
import qualified HN.Optimizer.ArgumentValues as AV
import qualified HN.Optimizer.ArgumentDeleter as AD

import HN.Optimizer.Node (Pass)
import Utils
import Debug.Trace
import HN.Optimizer.Visualise

optimizeHN libraryTypes = map (withGraph libraryTypes (fromTuple . runSimpleUniqueMonad . runWithFuel infiniteFuel . passes . toTuple))

oldInliner = runF >=> runB

newInliner = AV.runAv >=> AD.runF >=> AAD.runB

passes = oldInliner -- >=> oldInliner >=> oldInliner --AV.runAv >=> AD.runF-- >=> AAD.runB

noInliner :: Pass any ()
noInliner (x, _, _) = return (x, noFacts, undefined)

otrace _ x = x

toTuple agraph = (agraph, undefined, undefined)
fromTuple (agraph, facts, _) = otrace (formatGraph agraph ++ show facts) agraph

withGraph libNames f def = decompileGraph (xtrace "labelNames" labelNames) $ f graph where
	(graph, (_, labelNames)) = compileGraph libNames def
