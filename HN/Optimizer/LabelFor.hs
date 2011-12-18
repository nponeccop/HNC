module HN.Optimizer.LabelFor where

import Compiler.Hoopl
import Control.Arrow ((***))
import Control.Monad.State
import qualified Data.Map as M

run foo = runSimpleUniqueMonad $ runStateT foo (M.empty, M.empty)

freshLabelFor name = do
	l <- lift freshLabel
	modify $ M.insert name l *** M.insert l name
	return l

tracedUncondLookup m k = case M.lookup k m of
	Just a -> a
	Nothing -> error $ "LabelFor.tracedUncondLookup: " ++ show k ++ " not found"

labelFor () = liftM (tracedUncondLookup . fst) get

innerScope f = do
	a <- liftM fst get
	b <- f
	modify $ \(_, l2n) -> (a, l2n)
	return b
