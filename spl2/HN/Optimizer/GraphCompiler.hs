module HN.Optimizer.GraphCompiler (compileGraph) where

import Compiler.Hoopl

import HN.Intermediate
import Control.Monad.State
import HN.Optimizer.Node
import HN.Optimizer.LabelFor as LabelFor

compileGraph a def @ (Definition name _ _) = LabelFor.run $ do
	x <- freshLabelFor name
	libLabels <- mapM freshLabelFor a
	gg <- compileGraph5 def x
	return $ foldr (\x y -> node x LibNode |*><*| y) emptyClosedGraph libLabels |*><*| gg

compileGraph4 def @ (Definition name _ _) = do
	freshLabelFor name >>=	compileGraph5 def

compileGraph5 (Definition _ args letIn) x = innerScope $ do
		al <- mapM freshLabelFor args
		y <- compileLet letIn
		e <- compileExpr $ letValue letIn
		return $ node x (LetNode al e) |*><*| y |*><*| foldr (\x y -> argNode x |*><*| y) emptyClosedGraph al

compileLet (Let def letIn) = do
	y <- compileGraph4 def
	l <- compileLet letIn
	return $ l |*><*| y

compileLet (In _) = return emptyClosedGraph


-- compileLet :: LetIn -> LabelMapM ()
-- compileLet (In e) = compileExpr e
-- compileLet (Let def inner) = compileGraph def >> compileLet inner

compileExpr x = do
	aa <- labelFor ()
	return $ fmap aa x
