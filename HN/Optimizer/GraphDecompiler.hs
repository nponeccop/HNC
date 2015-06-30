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

decompileGraph :: M.Map Label String -> Graph N.Node C C -> Definition String
decompileGraph labelNames g = l2n <$> fromJust (decompiledGraph l2pd g firstLabel) where
	l2n l = uncondLookup l labelNames
	l2pd l = M.findWithDefault [] l $ graphPostdominators g

decompiledGraph l2pd g l = insertLet (mapMaybe (decompiledGraph l2pd g) $ l2pd l) <$> byLabel l where
	byLabel = lookupDefinition g

mapUncond k m = fromMaybe (error "mapUncond failed in GraphDecompiler") $ mapLookup k m

decompiledBlock :: Block N.Node C C -> N.DefinitionNode
decompiledBlock x = foldBlockNodesB f x undefined where
	f :: N.Node e x -> N.DefinitionNode -> N.DefinitionNode
	f (N.Entry _) o = o
	f (N.Exit d) _ = d

lookupDefinition :: Graph N.Node e x -> Label -> Maybe (Definition Label)
lookupDefinition (GMany _ l _) x = case decompiledBlock $ mapUncond x l of
	N.LetNode argLabels expr -> Just $ Definition x argLabels (In expr)
	_ -> Nothing
