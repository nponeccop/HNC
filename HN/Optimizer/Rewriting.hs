{-# LANGUAGE GADTs #-}
module HN.Optimizer.Rewriting (rewriteExpression, ListFact) where

import Data.Functor.Fixedpoint
import Data.Maybe
import Control.Applicative
import Compiler.Hoopl
import HN.Optimizer.Node
import HN.Optimizer.Visualise ()
import Utils

type Rewrite a = a -> Maybe a
{-
composeR :: Rewrite a -> Rewrite a -> Rewrite a
composeR a b x = if ch then Just result else Nothing where
	result = dropR b $ dropR a x
	ch = changed (a x) || changed (b x)
-}

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
	= Just . dropR (rewriteExpression2 $ flip mapUnion f $ mapFromList $ zip formalArgs $ map (PElem . LetNode []) actualArgs) 

rewriteExpression :: FactBase ListFact -> Rewrite (ExpressionFix)
rewriteExpression = rewriteMany . rewriteExpression2 
	
rewriteExpression2 :: FactBase ListFact -> Rewrite (ExpressionFix)
rewriteExpression2 f = process phi where
	phi (Constant _) = Nothing
	phi (Atom a) = do 
		([], e) <- processAtom "rewriteExpression2" a $ xtrace ("factBase-atom {" ++ show a ++ "}") f
		return e
	phi (Application (a, _) bb) | isAtom (a) = let
		-- b = map fst bb
		b' = map (uncurry fromMaybe) bb
		bChanged = any (isJust . snd) bb
		in case processAtom2 "rewriteApplication.Single" a f of
			Nothing -> if bChanged then Just $ Fix $ Application a b' else Nothing
			Just ([], expr) -> Just $ Fix $ Application expr b' 
			Just (args, expr) -> inlineApplication args b' f expr
	phi (Application (a, _) bb) | isAtomApplication (a) = rewriteApplication a (map fst bb) f
	phi (Application (a, _) bb) = let 
		b' = map (uncurry fromMaybe) bb 
		bChanged = any (isJust . snd) bb
		in case rewriteExpression2 f a of
			Nothing -> if bChanged then Just $ Fix $ Application a b' else Nothing 
			Just _ -> error "rapp.Just" 

processAtom err a f = case lookupFact a f of
	Nothing -> error $ err ++ ".uncondLookupFact.Nothing"
 	Just Bot -> error $ err ++ ".rewriteExitL.Bot"
 	Just (PElem (LetNode args body)) -> Just (args, body)
	_ -> Nothing
	
processAtom2 err (Fix (Atom a)) f = processAtom err a f 

process3 :: (Fix ExpressionFunctor -> c -> b) -> (ExpressionFunctor b -> c) -> Fix ExpressionFunctor -> c
process3 j f = self
	where self = f . fmap (\x -> j x (self x)) . unFix
	
process :: (ExpressionFunctor (ExpressionFix, Maybe ExpressionFix) -> Maybe ExpressionFix) -> Rewrite ExpressionFix
process = process3 (,) 
	
rewriteMany :: Rewrite a -> Rewrite a
rewriteMany clientRewrite x = clientRewrite x >>= rewriteAfterChange where
	rewriteAfterChange x = (clientRewrite x >>= rewriteAfterChange) <|> return x
