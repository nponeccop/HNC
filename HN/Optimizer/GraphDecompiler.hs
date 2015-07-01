{-# LANGUAGE GADTs, TupleSections #-}
module HN.Optimizer.GraphDecompiler (decompileGraph) where

import Compiler.Hoopl
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Map as M
import Data.Maybe
import Safe (fromJustNote)

import HN.Intermediate
import HN.Optimizer.Dominator
import HN.Optimizer.Node
import Utils

firstLabel = runSimpleUniqueMonad freshLabel

decompileGraph :: M.Map Label String -> MyGraph -> Definition String
decompileGraph labelNames g = fmap (`uncondLookup` labelNames ) 
	$ ana (mapWhere $ mapMaybe baz) $ fromJust $ baz firstLabel
	where
		baz :: Label -> Maybe (DefinitionBase Label Label)
		baz x = lookupValue2 g (M.findWithDefault [] x pd) x
		pd = graphPostdominators g

mapWhere f (DefinitionF a b v w) = DefinitionF a b v $ f w

mapUncond k m = fromJustNote "GraphDecompiler.mapUncond" $ mapLookup k m

decompiledBlock :: MyBlock -> DefinitionNode
decompiledBlock x = foldBlockNodesB f x undefined where
	f :: Node e x -> DefinitionNode -> DefinitionNode
	f (Entry _) o = o
	f (Exit d) _ = d

lookupValue2 (GMany _ l _) pd x = case decompiledBlock $ mapUncond x l of
	LetNode argLabels expr -> Just $ DefinitionF x argLabels expr pd
	_ -> Nothing
