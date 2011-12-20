module HN.MilnerTools (lookupAndInstantiate, constantType, freshAtoms, tv) where
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Utils
import HN.Intermediate
import SPL.Types

lookupAndInstantiate name table = instantiatedType $ tracedUncondLookup "MilnerTools.lookupAndInstantiate" name table

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

instantiatedType (tu, t) counter = (counter + S.size tu, substituteType t substitutions) where
	substitutions = M.fromDistinctAscList $ zipWith (\a b -> (a, tv b)) (S.toAscList tu) [counter..]

substituteType t substitutions = xtrace ("stv : " ++ show substitutions ++ " # " ++ show t) $ subst t where
	subst t = case t of
		TU a -> fromMaybe (TU a) (M.lookup a substitutions)
		TV a -> fromMaybe (TV a) (M.lookup a substitutions)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t
