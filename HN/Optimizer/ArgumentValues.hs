module HN.Optimizer.ArgumentValues where

import Compiler.Hoopl
import Data.Functor.Foldable
import qualified Data.Map as M

import HN.Optimizer.Node

type Fact = WithTopAndBot ExpressionFix

lattice = addPoints' "ArgumentValues" $ error "ArgumentValues.lattice"

varArgs a = case a of
	Atom _ -> M.empty
	Application (Fix (Atom var), _) bb -> let (xx, yy) = unzip bb in M.insertWith (++) var [xx] $ M.unions yy

process :: ExpressionFix -> M.Map Label [[ExpressionFix]]
process = para varArgs
