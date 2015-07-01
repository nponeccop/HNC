{-# LANGUAGE GADTs #-}
module HN.Optimizer.Utils where

import Compiler.Hoopl

import HN.Optimizer.Node

rewriteExitF :: (DefinitionNode -> f -> Maybe DefinitionNode) -> Node e x -> f -> Maybe (Graph Node e x)
rewriteExitF _ (Entry _) _ = Nothing
rewriteExitF rewriteDefinition (Exit n) f = mkLast . Exit <$> rewriteDefinition n f

rewriteExitB :: (DefinitionNode -> FactBase f -> Maybe DefinitionNode) -> Node e x -> Fact x f -> Maybe (Graph Node e x)
rewriteExitB rf (Entry _) _ = Nothing
rewriteExitB rf (Exit dn) f = mkLast . Exit <$> rf dn f

transferExitF :: (DefinitionNode -> f -> FactBase f) -> Node e x -> f -> Fact x f
transferExitF _ (Entry _) f = f
transferExitF tf (Exit n) f = tf n f

transferExitB :: (DefinitionNode -> FactBase f -> f)  -> Node e x -> Fact x f -> f
transferExitB _ (Entry _)  f = f
transferExitB tf (Exit n) f = tf n f

