{-# LANGUAGE GADTs #-}
module HN.Optimizer.Rewriting (rewriteExpression, ListFact) where

import qualified Data.Foldable as F
import Data.Functor.Foldable
import Data.Maybe
import Control.Applicative
import Compiler.Hoopl
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import Utils


type Rewrite a = a -> Maybe a

type ListFact = WithTopAndBot DefinitionNode	

dropR :: Rewrite a -> a -> a
dropR a x = fromMaybe x (a x)

rewriteApplication :: ExpressionFix -> [ExpressionFix] -> FactBase ListFact -> Maybe ExpressionFix

isAtom (Fix (Atom _)) = True
isAtom _ = False  

isAtomApplication (Fix (Application (Fix (Atom _)) _)) = True
isAtomApplication _ = False

rewriteApplication (Fix (Application a b)) c f = case processAtom2 "rewriteApplication.Double.1" a f of 
	Nothing -> Nothing
	Just ([], _) -> error "rewriteApplication.double.var"
	Just (outerParams, Fix (Atom aOuterBody)) -> case processAtom2 "rewriteApplication.Double.2" (Fix $ Atom aOuterBody) f of
		Just (innerParams, innerBody) -> fmap ff $ inlineApplication innerParams c f innerBody where
			ff (Fix (Application aa bb)) = Fix $ Application (dropR (inlineApplication outerParams b f) aa) bb  
			ff _ = error "rewriteApplication.double.fn.Just.noApp"				
		_ -> error "rewriteApplication.double.fn.Nothing"

inlineApplication formalArgs actualArgs f 
	= Just . dropR (rewriteExpression $ flip mapUnion f $ mapFromList $ zip formalArgs $ map (PElem . LetNode []) actualArgs) 

rewriteExpression :: FactBase ListFact -> Rewrite ExpressionFix
rewriteExpression = para . phi
	
phi :: FactBase ListFact -> ExpressionFunctor (ExpressionFix, Maybe ExpressionFix) -> Maybe ExpressionFix
phi _ (Constant _) = Nothing
phi f a @ (Atom _) = do 
	([], e) <- processAtom "rewriteExpression" a $ xtrace ("factBase-atom {" ++ show a ++ "}") f
	return e
phi f expr @ (Application (a, _) bb) | isAtom a = let
	b' = map (uncurry fromMaybe) bb
	in case processAtom2 "rewriteApplication.Single" a f of
		Nothing -> foo expr
		Just ([], expr) -> Just $ Fix $ Application expr b' 
		Just (args, expr) -> inlineApplication args b' f expr
phi f (Application (a, _) bb) | isAtomApplication a = rewriteApplication a (map fst bb) f
phi _ expr @ (Application _ _) = foo expr
	
foo (Application (a, _) bb) = if bChanged then Just $ Fix $ Application a b' else Nothing where
	b' = map (uncurry fromMaybe) bb 
	bChanged = any (isJust . snd) bb

processAtom err (Atom a) f = case lookupFact a f of
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"
 	Just Bot -> error $ err ++ ".rewriteExitL.Bot"
 	Just (PElem (LetNode args body)) -> Just (args, body)
	_ -> Nothing
	
unFix (Fix a) = a
	
processAtom2 err = processAtom err . unFix 

process2 :: (ExpressionFunctor ExpressionFix -> Maybe (Fix ExpressionFunctor)) -> Rewrite ExpressionFix
process2 f = para ff where
	ff x = let x' = uncurry fromMaybe <$> x
		in case f x' of
 			Nothing -> if F.any (isJust . snd) x then Just $ Fix x' else Nothing
			x -> x

phi2 :: FactBase ListFact -> ExpressionFunctor ExpressionFix -> Maybe (Fix ExpressionFunctor)
phi2 _ (Constant _) = Nothing
phi2 f a @ (Atom _) = do 
	([], e) <- processAtom "rewriteExpression" a $ xtrace ("factBase-atom {" ++ show a ++ "}") f
	return e
phi2 f (Application a b') | isAtom a = case processAtom2 "rewriteApplication.Single" a f of
	Nothing -> Nothing
	Just ([], expr) -> Just $ Fix $ Application expr b' 
	Just (args, expr) -> inlineApplication args b' f expr
-- phi2 f (Application a bb) | isAtomApplication (a) = rewriteApplication a bb f
-- phi2 f (Application a bb) = Nothing 
