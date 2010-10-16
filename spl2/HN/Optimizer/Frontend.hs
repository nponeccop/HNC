module HN.Optimizer.Frontend (optimize) where
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler
import HN.Intermediate
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.Inbound
import HN.Optimizer.Inliner2


runFB = runF >=> runB


test3 = transform $ runFB >=> runFB

transform tf = fromTuple . runSimpleUniqueMonad . runWithFuel 1000 . tf . toTuple . compileGraph where
	toTuple agraph = (agraph, undefined, undefined)
	fromTuple (agraph, _, _) = agraph

optimize :: Definition -> Definition
optimize = decompileGraph . test3