{-# LANGUAGE GADTs #-}
module HN.Optimizer.GraphDecompiler (decompileGraph) where

import Compiler.Hoopl
import HN.Optimizer.Visualise ()
import qualified Data.Map as M
import qualified HN.Optimizer.Node as N
import HN.Intermediate
import HN.Optimizer.Dominator
import Data.Maybe
import Utils

firstLabel = runSimpleUniqueMonad freshLabel

decompileGraph labelNames g @ (GMany _ l _) = insertLet (mapMaybe baz $ M.findWithDefault [] firstLabel $ graphPostdominators g) $ fromJust $ baz firstLabel where
		l2n l = uncondLookup l labelNames
		baz x = decompiledNode2 l2n x $ decompiledBlock $ mapUncond x l

mapUncond k m = fromMaybe (error "mapUncond failed in GraphDecompiler") $ mapLookup k m

decompiledBlock :: Block N.Node C C -> N.DefinitionNode
decompiledBlock x = foldBlockNodesB f x undefined where
	f :: N.Node e x -> N.DefinitionNode -> N.DefinitionNode
	f (N.Entry _) o = o
	f (N.Exit d) _ = d

decompiledNode2 l2n l x = case x of
	N.LetNode argLabels expr -> Just $ Definition (l2n l) (map l2n argLabels) $ In $ fmap l2n expr
	_ -> Nothing
