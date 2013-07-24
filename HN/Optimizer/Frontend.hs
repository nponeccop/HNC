module HN.Optimizer.Frontend (optimizeHN) where
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.Inbound (runF)
import HN.Optimizer.Inliner2 (runB)
import HN.Optimizer.WithGraph (withGraph)

optimizeHN libraryTypes = map (withGraph libraryTypes (fromTuple . runSimpleUniqueMonad . runWithFuel infiniteFuel . tf . toTuple))

runFB = runF >=> runB

tf = runFB >=> runFB where

toTuple agraph = (agraph, undefined, undefined)
fromTuple (agraph, _, _) = agraph
