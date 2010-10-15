{-# LANGUAGE GADTs #-}
module HN.Optimizer.GraphDecompiler (decompileGraph) where

import Compiler.Hoopl
import HN.Optimizer.Visualise ()
import qualified Data.Map as M
import HN.Optimizer.Node
import HN.Optimizer.Intermediate
import HN.Optimizer.Dominator

firstLabel = runSimpleUniqueMonad freshLabel

decompileGraph :: Graph Node C C -> Definition
decompileGraph g @ (GMany _ l _) = (insertLet foo $ decompiledNode firstLabel $ decompiledBlock $ case mapLookup firstLabel l of
		Just entry -> entry) where
			pd = graphPostdominators g
			foo = map (\l -> decompiledNode l $ bar l) $ M.findWithDefault [] firstLabel pd
			bar ll = decompiledBlock $ case mapLookup ll l of
				Just entry -> entry

decompiledBlock :: Block Node C C -> DefinitionNode
decompiledBlock x = foldBlockNodesB f x undefined where
	f :: Node e x -> DefinitionNode -> DefinitionNode
	f (Entry _) o = o
	f (Exit d) _ = d

decompiledNode l x = case x of
	LetNode argLabels expr -> Definition (show l) (map show argLabels) $ In $ fmap show expr
	ArgNode -> Definition "@arg!" [] $ In (Constant $ ConstInt 2)
