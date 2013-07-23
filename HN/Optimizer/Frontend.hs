module HN.Optimizer.Frontend (optimize) where
import HN.Intermediate
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.Inbound
import HN.Optimizer.Inliner2
import HN.Optimizer.WithGraph

import HN.Visualise

runFB = runF >=> runB

transform tf xxx = withGraph (fromTuple . runSimpleUniqueMonad . runWithFuel infiniteFuel . tf . toTuple) xxx  where
	toTuple agraph = (agraph, undefined, undefined)
	fromTuple (agraph, _, _) = agraph

optimize :: [String] -> Definition -> Definition
optimize xxx = transform (runFB >=> runFB) xxx