{-# LANGUAGE GADTs #-}
module HN.Optimizer.GraphDecompiler (decompileGraph2) where

import Compiler.Hoopl
import HN.Optimizer.Visualise ()
import qualified Data.Map as M
import HN.Optimizer.Node
import HN.Intermediate
import HN.Optimizer.Dominator
import Data.Maybe

firstLabel = runSimpleUniqueMonad freshLabel

decompileGraph2 labelNames g @ (GMany _ l _) = (insertLet foo $ fromJust $ decompiledNode2 l2n firstLabel $ decompiledBlock $ case mapLookup firstLabel l of
	Just entry -> entry) where
		pd = graphPostdominators g
		foo = mapMaybe (\l -> decompiledNode2 l2n l $ bar l) $ M.findWithDefault [] firstLabel pd
		bar ll = decompiledBlock $ case mapLookup ll l of
			Just entry -> entry
		l2n l = M.findWithDefault (show l) l labelNames

decompiledBlock :: Block Node C C -> DefinitionNode
decompiledBlock x = foldBlockNodesB f x undefined where
	f :: Node e x -> DefinitionNode -> DefinitionNode
	f (Entry _) o = o
	f (Exit d) _ = d

decompiledNode2 l2n l x = case x of
	LetNode argLabels expr -> Just $ Definition (l2n l) (map l2n argLabels) $ In $ fmap l2n expr
	_ -> Nothing
