module HN.Optimizer.Dominator where
import Compiler.Hoopl
import Compiler.Hoopl.Passes.Dominator
import qualified Data.Map as M
import HN.Optimizer.Pass
import HN.Optimizer.Node

type Foo n f t1 t2 = (Graph n C C, t1, t2)
						  -> SimpleFuelMonad
							   (Graph n C C, FactBase f, MaybeO C f)

runDominatorF :: Foo Node Doms t1 t2
runDominatorF = runPass (analyzeAndRewriteFwd domPass) (\_ entry -> mapSingleton entry domEntry)

graphPostdominators g = runSimpleUniqueMonad $ runWithFuel infiniteFuel $
	runDominatorF (g, undefined, undefined) >>= \(_, f, _) -> return $ immediatePostdominators $ immediateDominators f

immediatePostdominators :: LabelMap Label -> M.Map Label [Label]
immediatePostdominators l = xx where
	ll = mapToList l
	xx :: M.Map Label [Label]
	xx = foldr f M.empty ll
	f (k, v) =  M.insertWith (\_ o -> k : o) v [k]
