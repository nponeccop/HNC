module HN.Optimizer.FormalArgumentsDeleter (runB) where

import Compiler.Hoopl hiding ((<*>))
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.ArgumentValues (ArgFact)
import HN.Optimizer.Utils

rewriteB :: DefinitionNode -> FactBase ArgFact -> Maybe DefinitionNode
rewriteB = rewriteNode WithoutChildren $ \f n -> case n of
	Application aa @ (Atom a) b -> fmap (smartApplication aa . map fst) . rewrite WithChildren deleteArg
	    =<< zipExactMay b
	    =<< convertFact
	    =<< lookupFact a f
	_ -> Nothing

convertFact :: ArgFact -> Maybe [WithTopAndBot ExpressionFix]
convertFact ((PElem a, _), _) = Just a
convertFact _ = Nothing

smartApplication a [] = a
smartApplication a b = Application a b

deleteArg :: Rewrite [(ExpressionFix, WithTopAndBot ExpressionFix)]
deleteArg ((_, PElem _) : tail) = Just tail
deleteArg _ = Nothing

runB :: Pass ArgFact ArgFact
runB = runPassB PassParams
	{ ppConvertFacts = const . convertFactBase
	, ppTransfer = noTransferMapB
	, ppRewrite = pureBRewrite $ rewriteExitB rewriteB
	}
