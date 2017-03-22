{-# LANGUAGE FlexibleContexts, LambdaCase, NoMonomorphismRestriction, TypeFamilies #-}

module HN.Optimizer.GraphCompiler (compileGraph) where
import Compiler.Hoopl
import Control.Applicative
import Data.Functor.Foldable
import qualified Data.Map as M

import HN.Intermediate
import qualified HN.Optimizer.Node as N
import HN.Optimizer.LabelFor as LabelFor

compileGraph libraryTypes def @ (Definition name _ _) = LabelFor.run $ do
	x <- freshLabelFor name
	libLabels <- mapM freshLabelFor $ M.keys libraryTypes
	gg <- compileGraph5 def x
	return $ foldr (\x y -> N.node x N.LibNode |*><*| y) emptyClosedGraph libLabels |*><*| gg

compileGraph4 def @ (Definition name _ _) = freshLabelFor name >>=	compileGraph5 def

compileGraph5 (Definition _ args letIn) x = innerScope $ do
		al <- mapM freshLabelFor args
		y <- compileLet letIn
		e <- compileExpr $ letValue letIn
		return $ N.node x (N.LetNode al e) |*><*| y |*><*| foldr (\x y -> N.argNode x |*><*| y) emptyClosedGraph al

compileLet = cata $ \case
	LetF def letIn -> liftA2 (|*><*|) (compileGraph4 def) letIn
	InF _ -> return emptyClosedGraph

-- compileLet :: LetIn -> LabelMapM ()
-- compileLet (In e) = compileExpr e
-- compileLet (Let def inner) = compileGraph def >> compileLet inner

compileExpr x = (<$> x) <$> labelFor ()
