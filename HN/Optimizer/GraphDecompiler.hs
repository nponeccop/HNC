{-# LANGUAGE GADTs #-}
module HN.Optimizer.GraphDecompiler (decompileGraph) where

import Compiler.Hoopl
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Map as M
import Data.Maybe
import Safe (fromJustNote)

import HN.Intermediate
import HN.Optimizer.Dominator
import qualified HN.Optimizer.Node as N
import Utils

firstLabel = runSimpleUniqueMonad freshLabel

decompileGraph :: M.Map Label String -> Graph N.Node C C -> Definition String
decompileGraph labelNames g = fmap (`uncondLookup` labelNames ) 
	$ foo (flip (M.findWithDefault []) $ graphPostdominators g) (lookupValue g) firstLabel

lookupLet l2pd l2value = self where
	self l = insertLet (mapMaybe self $ l2pd l) <$> l2value l

foo :: (Label -> [Label]) -> (Label -> Maybe (Definition Label)) -> Label -> Definition Label
foo l2pd l2value = ana phi where
	phi :: Label -> DefinitionBase Label Label
	phi x = DefinitionF a b v foo where
		foo = mapMaybe quux $ l2pd x
		(DefinitionF a b v []) = project $ fromJust $ l2value x
		quux x = const x <$> l2value x

mapUncond k m = fromJustNote "GraphDecompiler.mapUncond" $ mapLookup k m

decompiledBlock :: Block N.Node C C -> N.DefinitionNode
decompiledBlock x = foldBlockNodesB f x undefined where
	f :: N.Node e x -> N.DefinitionNode -> N.DefinitionNode
	f (N.Entry _) o = o
	f (N.Exit d) _ = d

lookupValue :: Graph N.Node e x -> Label -> Maybe (Definition Label)
lookupValue (GMany _ l _) x = case decompiledBlock $ mapUncond x l of
	N.LetNode argLabels expr -> Just $ Definition x argLabels (In expr)
	_ -> Nothing
