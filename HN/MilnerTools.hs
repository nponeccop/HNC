module HN.MilnerTools (lookupAndInstantiate, closure, constantType, freshAtoms, tv, instantiatedType, closure4) where
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Utils
import HN.Intermediate
import HN.TypeTools
import SPL.Types


lookupAndInstantiate :: String -> M.Map String T -> Int -> (Int, T)
lookupAndInstantiate name table counter = let t = tracedUncondLookup "MilnerTools.lookupAndInstantiate" name table in instantiatedType t counter

tv x = TV $ 't' : show x

-- freshAtoms используется всего в одном месте - при
-- вычислении атрибута Definition.loc.argAtoms
freshAtoms :: [String] -> Int -> (Int, [(String, T)])
freshAtoms [] counter = (counter, [])
freshAtoms a counter = (counter + length a, zipWith (\a i -> (a, tv i)) a [counter..])

-- используется при выводе типа константы в качестве tau
constantType x = case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"

closure env t = if S.null varsToSubst then t else mapTypeTV mapper t where
	varsToSubst = fst $ closure4 env t
	mapper a = xtrace "closure.mapper!" $ if S.member a varsToSubst then xtrace "Closure.TU" (TU a) else TV a

closure4 env t = (varsToSubst, t) where
	tpv = typeTv t
	epv = xtrace "closure.epv" $ envPolyVars env
	varsToSubst = xtrace "closure.varsToSubst" $ tpv S.\\ epv
	envPolyVars = M.fold f S.empty where
		f el acc = S.union acc $ typeTv el


instantiatedType :: T -> Int -> (Int, T)
instantiatedType t counter = (counter + length tu, substituteType t substitutions) where
	tu = S.toAscList $ typeTu t
	substitutions = M.fromDistinctAscList $ zipWith (\a b -> (a, tv b)) tu [counter..]

substituteType t substitutions = xtrace ("stv : " ++ show substitutions ++ " # " ++ show t) $ subst t where
	subst t = case t of
		TU a -> fromMaybe (TU a) (M.lookup a substitutions)
		TV a -> fromMaybe (TV a) (M.lookup a substitutions)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t
