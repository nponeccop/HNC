{-# LANGUAGE Rank2Types #-}
module HN.Optimizer.Pass (runPass, pureFRewrite, pureBRewrite, defaultFactsHack) where
import Compiler.Hoopl

pureBRewrite :: FuelMonad m => (forall e x . n e x -> Fact x f -> Maybe (Graph n e x)) -> BwdRewrite m n f
pureBRewrite ff = mkBRewrite $ \a b -> return $ ff a b

pureFRewrite :: FuelMonad m => (forall e x . n e x -> f -> Maybe (Graph n e x)) -> FwdRewrite m n f
pureFRewrite ff = mkFRewrite $ \a b -> return $ ff a b

defaultFactsHack l n = zip (successors n) $ repeat $ fact_bot l

runPass f makeFacts (graph, facts, _) = f (JustC [firstLabel]) graph $ makeFacts facts firstLabel where
	firstLabel = runSimpleUniqueMonad freshLabel
