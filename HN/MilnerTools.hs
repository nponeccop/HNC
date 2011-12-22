{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module HN.MilnerTools (instantiatedType, freshAtoms, MyStack, unifyM, runStack, subst, closureM, templateArgs, T(..), emptyClosureM, constantType, convertTv) where
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Unification
import Control.Unification.IntVar
import Data.Traversable (Traversable)
import Data.Foldable
import Control.Monad.State
import Control.Monad.Error
import Data.Tuple

import Utils
import HN.TypeTools
import HN.Intermediate (Const (..))
import qualified SPL.Types as Old

-- freshAtoms используется всего в одном месте - при
-- вычислении атрибута Definition.loc.argAtoms
freshAtoms :: [String] -> Int -> (Int, [(String, Old.T)])
freshAtoms [] counter = (counter, [])
freshAtoms a counter = (counter + length a, zipWith (\a i -> (a, tv i)) a [counter..])

instantiatedType counter (tu, t) = (counter + S.size tu, convert $ mapTypeTV (\a -> fromMaybe (Old.TV a) (M.lookup a substitutions)) t) where
	substitutions = M.fromDistinctAscList $ zipWith (\a b -> (a, tv b)) (S.toAscList tu) [counter..]


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
convert a @ (Old.TV _) = convertTv a

convertTv (Old.TV a) = do
	m <- xget
	fmap MutVar $ maybe (xfreeVar a m) (return . IntVar) $ M.lookup a m where
		xfreeVar a m = do
			ii @ (IntVar i) <- freeVar
			xput (M.insert a i m)
			return ii

reverseMap x = M.fromList $ map swap $ M.toList x

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

runApply = fmap fromRight . runErrorT . applyBindings

revertM newTerm = fmap (revert newTerm . reverseMap) xget

subst = convertAndBind >=> revertM

convertAndBind = convert >=> runApply

closureM2 inferredTypes tau = do
	convEnv <- mapM (convertAndBind . snd) $ M.elems inferredTypes
	convTau <- tau >>= runApply
	rm <- fmap reverseMap xget
	let varListToSet = fmap (S.fromList . map (\(IntVar x) -> x))
	tpv <- varListToSet $ getFreeVars convTau
	epv <- varListToSet $ fmap Prelude.concat $ mapM getFreeVars convEnv
	let revertTv x = tracedUncondLookup "closureM" x rm
	return (S.map revertTv $ tpv S.\\ epv, revert convTau rm)

closureM env argAtoms letTauM = closureM2 env $ do
	args <- mapM (convertTv . snd) argAtoms
	result <- letTauM
	return $ (MutTerm . TT) $ args ++ [result]

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
	runErrorT $ unify fn $ MutTerm $ TT $ args ++ [result]
	return result


-- используется при выводе типа константы в качестве tau
constantType x = return $ MutTerm $ case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"
