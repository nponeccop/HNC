{-# LANGUAGE GADTs #-}
module HN.Optimizer.Utils where

import Compiler.Hoopl

import HN.Optimizer.Node
type PureNodeRewrite e x = Node e x -> Maybe (Graph Node e x)

rewriteExit :: (DefinitionNode -> f -> Maybe DefinitionNode) -> Node e x -> f -> Maybe (Graph Node e x)
rewriteExit _ (Entry _) _ = Nothing
rewriteExit rewriteDefinition (Exit n) f = mkLast . Exit <$> rewriteDefinition n f

transferExitF :: (DefinitionNode -> f -> FactBase f) -> Node e x -> f -> Fact x f
transferExitF _ (Entry _) f = f
transferExitF tf (Exit n) f = tf n f
