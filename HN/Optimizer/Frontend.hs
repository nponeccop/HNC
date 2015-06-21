module HN.Optimizer.Frontend (optimizeHN, withGraph) where
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler
import HN.Optimizer.Inbound (runF)
import HN.Optimizer.Inliner2 (runB)
import Utils

optimizeHN libraryTypes = map (withGraph libraryTypes (fromTuple . runSimpleUniqueMonad . runWithFuel infiniteFuel . (runFB >=> runFB) . toTuple))

runFB = runF >=> runB

toTuple agraph = (agraph, undefined, undefined)
fromTuple (agraph, _, _) = agraph

withGraph libNames f def = decompileGraph (xtrace "labelNames" labelNames) $ f graph where
	(graph, (_, labelNames)) = compileGraph libNames def
