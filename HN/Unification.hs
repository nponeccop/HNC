{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module HN.Unification  (MyStack, unifyM, runStack, subst, closureM, templateArgs) where

import Control.Unification
import Control.Unification.IntVar
import Data.Traversable (Traversable)
import Data.Foldable
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Tuple

import Utils
import qualified SPL.Types as Old


data T a = T String | TT [a] | TD String [a] | TU String deriving (Eq, Show, Functor, Traversable, Foldable)

maybeZip a b | length a == length b = Just $ zip a b
maybeZip _ _ = Nothing

instance Unifiable T where
	zipMatch (TT a1) (TT a2) = fmap TT $ maybeZip a1 a2
	zipMatch (T a) (T b) | a == b = Just $ T a
	zipMatch (TU a) (TU b) | a == b = Just $ TU a
	zipMatch (TD l1 a1) (TD l2 a2) | l1 == l2 = fmap (TD l1) $ maybeZip a1 a2
	zipMatch _ _ = Nothing

xget :: MyStack (M.Map String Int)
xget = lift get

xput :: M.Map String Int -> MyStack ()
xput = lift . put

type MyStack a = IntBindingT T (State (M.Map String Int)) a

runStack x = fst $ fst $ flip runState (M.empty :: M.Map String Int) $ runIntBindingT x

convert (Old.T a) = return $ MutTerm $ T a
convert (Old.TT a) = fmap (MutTerm . TT) $ Prelude.mapM convert a
convert (Old.TD n a) = fmap (MutTerm . TD n) $ Prelude.mapM convert a
convert (Old.TU a) = return $ MutTerm $ TU a
convert (Old.TV a) = do
	m <- xget
	fmap MutVar $ maybe (xfreeVar a m) (return . IntVar) $ M.lookup a m where
		xfreeVar a m = do
			ii @ (IntVar i) <- freeVar
			xput (M.insert a i m)
			return ii


reverseMap x = M.fromList $ map swap $ M.toList x

unifyM x y = do
	a <- convert x
	b <- convert y
	runErrorT $ unify a b >>= applyBindings

subsumesM x y = runErrorT (subsumes x y) >> exportBindings

exportBindings = do
	x <- fmap reverseMap xget
	xget >>= fmap (M.fromList . catMaybes) . mapM (fff x) . M.toList where
		fff x (tv, iv) = fmap (fmap (\ o -> (tv, revert o x))) $ lookupVar $ IntVar iv

revert x m = mrevert x where
	mrevert (MutTerm x) = f x
	mrevert (MutVar (IntVar i)) = Old.TV $ tracedUncondLookup "Unification.revert" i m
	f (T x) = Old.T x
	f (TU x) = Old.TU x
	f (TT x) = Old.TT $ map mrevert x
	f (TD s x) = Old.TD s $ map mrevert x

revert2 newTerm = fmap (revert newTerm . reverseMap) xget

runApply = fmap fromRight . runErrorT . applyBindings

subst = convert >=> runApply >=> revert2

closureM inferredTypes tau = do
	convEnv <- mapM ((convert >=> runApply) . snd) $ M.elems inferredTypes
	convTau <- (convert >=> runApply) tau
	rm <- fmap reverseMap xget
	let varListToSet = fmap (S.fromList . map (\(IntVar x) -> x))
	tpv <- varListToSet $ getFreeVars convTau
	epv <- varListToSet $ fmap Prelude.concat $ mapM getFreeVars convEnv
	let revertTv x = tracedUncondLookup "closureM" x rm
	return (S.map revertTv $ tpv S.\\ epv, revert convTau rm)

templateArgs tau (generalizedVars, generalizedT) = do
	inferredType <- convert generalizedT
	callSiteType <- convert tau >>= runApply
	subst2 <- subsumesM inferredType callSiteType
	let fs x = tracedUncondLookup "AG.TypeInference.fs" x subst2
	return $ map fs $ S.toList generalizedVars where
