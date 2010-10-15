module HN.Optimizer.GraphCompiler (compileGraph) where

import Compiler.Hoopl

import HN.Optimizer.Intermediate
import Control.Monad.State
import HN.Optimizer.Node
import HN.Optimizer.LabelFor as LabelFor

compileGraph :: Definition -> Graph Node C C
compileGraph def = fst $ LabelFor.run $ compileGraph4 def

compileGraph4 (Definition name args letIn) = do
	x <- freshLabelFor name
	innerScope $ do
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

compileExpr (Constant x) = return $ Constant x
compileExpr (Atom a) = fmap  Atom $ labelFor a
compileExpr (Application a b) = liftM2 Application (compileExpr a) (mapM compileExpr b)


