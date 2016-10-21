{-# LANGUAGE FlexibleContexts #-}
module HN.Optimizer.LabelFor where

import Compiler.Hoopl
import Control.Arrow ((***))
import Control.Monad.State
import qualified Data.Map as M
import Utils

run foo = runSimpleUniqueMonad $ runStateT foo (M.empty, M.empty)

freshLabelFor name = do
	l <- lift freshLabel
	modify $ M.insert name l *** M.insert l name
	return l

labelFor () = tracedUncondLookup2 . fst <$> get where
	tracedUncondLookup2 = flip $ tracedUncondLookup "LabelFor.tracedUncondLookup: "


innerScope f = do
	a <- fst <$> get
	b <- f
	modify $ \(_, l2n) -> (a, l2n)
	return b
