{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module HN.Unification  (mySubsumes, MyStack, xxunify, closure2, runStack, subst2, revert2, runApply, convert, convertTU) where

import Control.Unification
import Control.Unification.IntVar
import Data.Traversable (Traversable)
import Data.Foldable
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple
import qualified Data.Traversable as DT

import Utils
import HN.MilnerTools (closure)
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

xunify x y = runErrorT $ unify x y >>= applyBindings
xsubsumes x y = runErrorT $ subsumes x y

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

xxunify x y = do
	a <- convert x
	b <- convert y
	xunify a b

mySubsumes a b = xsubsumes a b >> exportBindings

convertTU = convert . f where
	f (Old.TU x) = Old.TV x
	f (Old.TT x) = Old.TT $ map f x
	f (Old.TD a x) = Old.TD a $ map f x
	f x = x


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

subst = subst2 >=> revert2

subst2 = convert >=> runApply

runApply = fmap fromRight . runErrorT . applyBindings

closure2 inferredTypes tau = liftM2 closure (DT.mapM subst inferredTypes) (subst tau)
