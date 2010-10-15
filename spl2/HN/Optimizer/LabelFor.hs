module HN.Optimizer.LabelFor where

import Compiler.Hoopl
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

run foo = runSimpleUniqueMonad $ runStateT foo M.empty

freshLabelFor name = do
	l <- lift freshLabel
	modify $ M.insert name l
	return l

labelFor name = fmap (fromJust . M.lookup name) get

innerScope f = do
	a <- get
	b <- f
	put a
	return b