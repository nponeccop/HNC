module HN.Optimizer.LabelFor where

import Compiler.Hoopl
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

run foo = runSimpleUniqueMonad $ runStateT foo (M.empty, M.empty)

freshLabelFor name = do
	l <- lift freshLabel
	modify $ \(n2l, l2n) -> (M.insert name l n2l, M.insert l name l2n)
	return l

tracedUncondLookup k m = case M.lookup k m of
	Just a -> a
	Nothing -> error $ "LabelFor.tracedUncondLookup: " ++ show k ++ " not found"

labelFor name = fmap (tracedUncondLookup name . fst) get

innerScope f = do
	a <- fmap fst get
	b <- f
	modify $ \(_, l2n) -> (a, l2n)
	return b