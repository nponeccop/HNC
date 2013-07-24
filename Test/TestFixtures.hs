module Test.TestFixtures (test1, test2, test3, testDominators, testPostdominators, compilerTest, decompilerTest) where
import Compiler.Hoopl
import Control.Monad
import qualified Data.Map as M
import HN.Optimizer.GraphCompiler
import HN.Optimizer.Inliner2
import HN.Optimizer.Inbound
import HN.Optimizer.Visualise (foo)
import HN.Optimizer.Dominator
import Compiler.Hoopl.Passes.Dominator
import HN.Optimizer.Frontend (withGraph)

cg = fst . compileGraph (M.singleton "incr" $ error "TestFixtures.cg") 

compilerTest = foo . cg

runFB = runF >=> runB

test2 = transform runFB

test3 = transform $ runFB >=> runFB

transform tf = foo . fromTuple . bar tf where
	fromTuple (agraph, _, _) = agraph

test1 = testFacts id runF

testDominators = testFacts immediateDominators runDominatorF

testFacts f r = show . f . (\(_, oFacts, _ ) -> oFacts) . bar r

testPostdominators = testFacts (immediatePostdominators . immediateDominators) runDominatorF

bar rf1 = runSimpleUniqueMonad . runWithFuel 1000 . rf1 . toTuple . cg where
	toTuple agraph = (agraph, undefined, undefined)

decompilerTest = withGraph M.empty id 
