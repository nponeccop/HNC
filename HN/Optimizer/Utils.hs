{-# LANGUAGE GADTs #-}
module HN.Optimizer.Utils where

import Compiler.Hoopl
import qualified Data.Map as M

import HN.Optimizer.ClassyLattice
import HN.Optimizer.Node

rewriteExitF :: (DefinitionNode -> f -> Maybe DefinitionNode) -> Node e x -> f -> Maybe (Graph Node e x)
rewriteExitF _ (Entry _) _ = Nothing
rewriteExitF rewriteDefinition (Exit n) f = mkLast . Exit <$> rewriteDefinition n f

rewriteExitB :: (DefinitionNode -> FactBase f -> Maybe DefinitionNode) -> Node e x -> Fact x f -> Maybe (Graph Node e x)
rewriteExitB _ (Entry _) _ = Nothing
rewriteExitB rf (Exit dn) f = mkLast . Exit <$> rf dn f

transferExitF :: (DefinitionNode -> f -> FactBase f) -> Node e x -> f -> Fact x f
transferExitF _ (Entry _) f = f
transferExitF tf (Exit n) f = tf n f

transferExitB :: (DefinitionNode -> FactBase f -> f)  -> Node e x -> Fact x f -> f
transferExitB _ (Entry _)  f = f
transferExitB tf (Exit n) f = tf n f

type MapFact f = (f, M.Map Label f)

transferMapExitF :: Lattice f => (DefinitionNode -> f -> [(Label, f)]) -> Node e x -> MapFact f -> Fact x (MapFact f)
transferMapExitF _ (Entry l) (curFact, factBase) = newFact where
	update newFact = (newFact, M.insert l newFact factBase)
	baseFact = M.lookup l factBase
	newFact = case baseFact of
		Nothing -> update curFact
		Just baseFact -> case join (OldFact baseFact) (NewFact curFact) of
			Nothing -> (baseFact, factBase)
			Just newFact -> update newFact

transferMapExitF tf nn @ (Exit n) (f, m) = distributeFact nn $ (,) bot $ mereJoin m $ M.fromList $ tf n f where

noTransferMapF :: Lattice f => FwdTransfer Node (MapFact f)
noTransferMapF = mkFTransfer $ transferMapExitF (\_ _ -> [])
