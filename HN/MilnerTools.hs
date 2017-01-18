{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveTraversable, DeriveFoldable, NoMonomorphismRestriction, FlexibleContexts #-}
module HN.MilnerTools (instantiatedType, freshAtoms, MyStack, unifyM, runStack, subst, closureM, templateArgs, T(..), emptyClosureM, constantType, convertTv, convert, getReverseMap, revert, runApply, UTerm(..), IntVar(..)) where
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Unification (Fallible(..), lookupVar, applyBindings, getFreeVars, freeVar)
import Control.Unification.IntVar (IntVar(..), IntBindingT, runIntBindingT)
import Control.Monad.State
import Control.Monad.Trans.Except
import Utils
import HN.TypeTools
import HN.Intermediate (Const (..))
import qualified SPL.Types as Old
import Unifier.Unifier
-- freshAtoms используется всего в одном месте - при
-- вычислении атрибута Definition.loc.argAtoms
-- argument types are not generalized, thus S.empty
freshAtoms a counter = zipWith (\a i -> (a, (S.empty, tv i))) a [counter..]

instantiatedType counter (tu, t) = (counter + S.size tu, convert $ mapTypeTV (\a -> fromMaybe (Old.TV a) (M.lookup a substitutions)) t) where
	substitutions = M.fromDistinctAscList $ zipWith (\a b -> (a, tv b)) (S.toAscList tu) [counter..]

instance Fallible T IntVar String where
	occursFailure _ _ = "ooo"
	mismatchFailure _ _ = "mmm"

xget :: MyStack (M.Map String Int)
xget = lift get

xput :: M.Map String Int -> MyStack ()
xput = lift . put

type MyStack a = IntBindingT T (State (M.Map String Int)) a

runStack x = fst $ fst $ flip runState (M.empty :: M.Map String Int) $ runIntBindingT x

convert (Old.T a) = return $ UTerm $ T a
convert (Old.TT a) = (UTerm . TT) <$> Prelude.mapM convert a
convert (Old.TD n a) = (UTerm . TD n) <$> Prelude.mapM convert a
convert a @ (Old.TV _) = convertTv a

convertTv (Old.TV a) = do
	m <- xget
	fmap UVar $ maybe (xfreeVar a m) (return . IntVar) $ M.lookup a m where
		xfreeVar a m = do
			ii @ (IntVar i) <- freeVar
			xput (M.insert a i m)
			return ii

subsumesM x y = runErrorT2 (subsumes x y) >> exportBindings

runErrorT2 :: ExceptT String m a -> m (Either String a)
runErrorT2 = runExceptT

exportBindings = do
	x <- fmap reverseMap xget
	xget >>= fmap (M.fromList . catMaybes) . mapM (fff x) . M.toList where
		fff x (tv, iv) = fmap (fmap (\ o -> (tv, revert o x))) $ lookupVar $ IntVar iv

revert x m = mrevert x where
	mrevert (UTerm x) = f x
	mrevert (UVar (IntVar i)) = Old.TV $ tracedUncondLookup "Unification.revert" i m
	f (T x) = Old.T x
	f (TT x) = Old.TT $ map mrevert x
	f (TD s x) = Old.TD s $ map mrevert x

runApply = fmap fromRight . runErrorT2 . applyBindings

revertM newTerm = fmap (revert newTerm . reverseMap) xget

subst = convertAndBind >=> revertM

convertAndBind = convert >=> runApply

getReverseMap = fmap reverseMap xget

closureM = liftM3M $ \convEnv args result -> do
	let convTau = UTerm $ TT $ args ++ [result]
	let varListToSet = fmap (S.fromList . map (\(IntVar x) -> x))
	tpv <- varListToSet $ getFreeVars convTau
	epv <- varListToSet $ Prelude.concat <$> mapM getFreeVars convEnv
	return (tpv S.\\ epv, convTau)

emptyClosureM tau = do
	convTau <- tau >>= runApply >>= revertM
	return (S.empty, convTau)

templateArgs tau (generalizedVars, generalizedT) = do
	inferredType <- convert generalizedT
	callSiteType <- tau >>= runApply
	subst2 <- subsumesM inferredType callSiteType
	let fs x = tracedUncondLookup "AG.TypeInference.fs" x subst2
	return $ map fs $ S.toList generalizedVars

unifyM fnTau argTau beta = do
	args <- sequence argTau
	result <- beta
	fn <- fnTau
	runErrorT2 $ unify fn $ UTerm $ TT $ args ++ [result]
	return result


-- используется при выводе типа константы в качестве tau
constantType x = return $ UTerm $ case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"
