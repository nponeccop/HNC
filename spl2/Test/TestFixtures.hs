module Test.TestFixtures (test1, test2, test3, test4, compilerTest, decompilerTest) where
import Compiler.Hoopl
import Control.Monad
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler
import HN.Optimizer.Inliner2
import HN.Optimizer.Inbound
import HN.Optimizer.Visualise (foo)
import HN.Optimizer.Dominator
import Compiler.Hoopl.Passes.Dominator

compilerTest = foo . compileGraph

runFB = runF >=> runB

test2 = transform runFB

test3 = transform $ runFB >=> runFB

transform tf = foo . fromTuple . bar tf where
	fromTuple (agraph, _, _) = agraph


test1 = testFacts id runF

test4 = testFacts immediateDominators runDominatorF

testFacts f r = show . f . (\(_, oFacts, _ ) -> oFacts) . bar r


bar rf1 = runSimpleUniqueMonad . runWithFuel 1000 . rf1 . toTuple . compileGraph where
	toTuple agraph = (agraph, undefined, undefined)

decompilerTest = decompileGraph . compileGraph