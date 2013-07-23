module HN.Optimizer.Frontend (optimize) where
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.Inbound (runF)
import HN.Optimizer.Inliner2 (runB)
import HN.Optimizer.WithGraph (withGraph)

runFB = runF >=> runB

transform tf = withGraph (fromTuple . runSimpleUniqueMonad . runWithFuel infiniteFuel . tf . toTuple)  where
	toTuple agraph = (agraph, undefined, undefined)
	fromTuple (agraph, _, _) = agraph

optimize = transform (runFB >=> runFB)