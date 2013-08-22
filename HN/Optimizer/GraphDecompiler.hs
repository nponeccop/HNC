{-# LANGUAGE GADTs #-}
module HN.Optimizer.GraphDecompiler (decompileGraph) where

import Data.Functor.Foldable
import Compiler.Hoopl
import HN.Optimizer.Visualise ()
import qualified Data.Map as M
import qualified HN.Optimizer.Node as N
import HN.Intermediate
import HN.Optimizer.Dominator
import Data.Maybe

firstLabel = runSimpleUniqueMonad freshLabel

decompileGraph labelNames g @ (GMany _ l _) = (insertLet foo $ fromJust $ decompiledNode2 l2n firstLabel $ decompiledBlock $ case mapLookup firstLabel l of
	Just entry -> entry) where
		pd = graphPostdominators g
		foo = mapMaybe (\l -> decompiledNode2 l2n l $ bar l) $ M.findWithDefault [] firstLabel pd
		bar ll = decompiledBlock $ case mapLookup ll l of
			Just entry -> entry
		l2n l = M.findWithDefault (show l) l labelNames

decompiledBlock :: Block N.Node C C -> N.DefinitionNode
decompiledBlock x = foldBlockNodesB f x undefined where
	f :: N.Node e x -> N.DefinitionNode -> N.DefinitionNode
	f (N.Entry _) o = o
	f (N.Exit d) _ = d

decompiledNode2 l2n l x = case x of
	N.LetNode argLabels expr -> Just $ Definition (l2n l) (map l2n argLabels) $ In $ decompiledExpr l2n expr
	_ -> Nothing

decompiledExpr l2n = cata $ \x -> case x of
	N.ApplicationF a b -> Application a b 
	N.ConstantF a -> Constant a
	N.AtomF a -> Atom $ l2n a