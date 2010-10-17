{-# LANGUAGE GADTs #-}
module HN.Optimizer.GraphDecompiler (decompileGraph, decompileGraph2) where

import Compiler.Hoopl
import HN.Optimizer.Visualise ()
import qualified Data.Map as M
import HN.Optimizer.Node
import HN.Intermediate
import HN.Optimizer.Dominator
import Data.Maybe

firstLabel = runSimpleUniqueMonad freshLabel

xfromJust _ (Just x) = x
xfromJust e Nothing = error $ "xfromJust:" ++ e

decompileGraph2 labelNames g @ (GMany _ l _) = (insertLet foo $ fromJust $ decompiledNode2 l2n firstLabel $ decompiledBlock $ case mapLookup firstLabel l of
	Just entry -> entry) where
		pd = graphPostdominators g
		foo = mapMaybe (\l -> decompiledNode2 l2n l $ bar l) $ M.findWithDefault [] firstLabel pd
		bar ll = decompiledBlock $ case mapLookup ll l of
			Just entry -> entry
		l2n l = M.findWithDefault (show l) l labelNames

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
	LibNode -> Definition "@lib!" [] $ In (Constant $ ConstInt 5)

decompiledNode2 l2n l x = case x of
	LetNode argLabels expr -> Just $ Definition (l2n l) (map l2n argLabels) $ In $ fmap l2n expr
	_ -> Nothing
