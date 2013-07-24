module HN.Optimizer.Frontend (optimizeHN) where
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.Inbound (runF)
import HN.Optimizer.Inliner2 (runB)
import HN.Optimizer.WithGraph (withGraph)

runFB = runF >=> runB

transform tf libraryTypes = withGraph (fromTuple . runSimpleUniqueMonad . runWithFuel infiniteFuel . tf . toTuple) libraryTypes where
	toTuple agraph = (agraph, undefined, undefined)
	fromTuple (agraph, _, _) = agraph

optimizeHN libraryTypes = map (transform (runFB >=> runFB) libraryTypes)
