{-# LANGUAGE GADTs #-}
module HN.Optimizer.Utils where

import Compiler.Hoopl
import qualified Data.Map as M
import Data.Maybe

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

transferMapExitB :: Lattice f => (DefinitionNode -> FactBase f -> f) -> Node e x -> Fact x (MapFact f) -> MapFact f
transferMapExitB _ (Entry l) o @ (curFact, factBase) = newFact where
	baseFact = M.lookup l factBase
	newFact = case baseFact of
		Nothing -> (curFact, M.insert l curFact factBase)
		Just baseFact -> case join (OldFact baseFact) (NewFact curFact) of
			Nothing -> (baseFact, factBase)
			Just newFact -> (newFact, M.insert l newFact factBase)
transferMapExitB tf (Exit dn) f = (tf dn (mapMap fst $ convertFactBase f), bot)

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

noTransferMapB :: Lattice f => BwdTransfer Node (MapFact f)
noTransferMapB = mkBTransfer $ transferMapExitB (\_ _ -> bot)

convertFactBase :: Lattice f => FactBase (MapFact f) -> FactBase (MapFact f)
convertFactBase f = mapSquare $ foldr foo M.empty $ concatMap ff $ mapToList f where
	ff (l, (f, m)) = (l, f) : M.toList m
	foo (l, f) = M.insertWith mereJoin l f

mapSquare1 :: Lattice f => M.Map Label f -> Label -> MapFact f
mapSquare1 m l = (fromMaybe bot $ M.lookup l m, m)

mapSquare :: Lattice f => M.Map Label f -> FactBase (MapFact f)
mapSquare m = mapFromList $ map ff $ M.keys m where
	ff l = (l, mapSquare1 m l)

instance Functor LabelMap where
	fmap = mapMap
