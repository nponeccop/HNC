{-# LANGUAGE GADTs, TypeFamilies, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances #-}
module HN.Optimizer.FormalArgumentsDeleter (runB) where

import Compiler.Hoopl hiding ((<*>))
import qualified Control.Monad as CM
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.ArgumentValues (ArgFact, argLattice, AFType)
import HN.Optimizer.Utils
import Utils

transferB :: DefinitionNode -> FactBase AFType -> AFType
transferB _ _ = bot

rewriteB :: DefinitionNode -> FactBase ArgFact -> Maybe DefinitionNode
rewriteB xll f = case xll of
	LibNode -> Nothing
	ArgNode -> Nothing
	LetNode l expr -> LetNode l <$> process' (rewriteExpression $ mapMapWithKey convertFact $ xtrace "rewrite.f" f) expr

convertFact :: Label -> ArgFact -> Maybe [WithTopAndBot ExpressionFix]
convertFact l ((callFact, _), _) = case xtrace "callFact" callFact of
	PElem a -> Just a
	Top -> Nothing

rewriteExpression f (Application aa @ (Atom a) b) = smartApplication aa <$> (rewriteArguments (xtrace "rewriteExpression.args" b) $ xtrace "rewriteExpression.fact" $ CM.join $ lookupFact a f)
rewriteExpression _ _ = Nothing

rewriteArguments b f = map fst <$> (process deleteArg =<< zipExactMay b =<< f)

smartApplication a [] = a
smartApplication a b = Application a b

deleteArg :: Rewrite [(ExpressionFix, WithTopAndBot ExpressionFix)]
deleteArg ((_, PElem _) : tail) = Just tail
deleteArg _ = Nothing

passB = BwdPass
	{ bp_lattice = argLattice
	, bp_transfer = mkBTransfer $ transferMapExitB transferB
	, bp_rewrite = pureBRewrite $ rewriteExitB rewriteB
	}

runB :: Pass ArgFact ArgFact
runB = runPass (analyzeAndRewriteBwd passB) (const . convertFactBase)

instance Functor LabelMap where
	fmap = mapMap
