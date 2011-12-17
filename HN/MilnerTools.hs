module HN.MilnerTools where
import Control.Arrow (second)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Utils
import HN.Intermediate
import HN.TypeTools
import HN.Unification
import SPL.Types
import SPL.Visualise


lookupAndInstantiate :: String -> M.Map String T -> Int -> (Int, T)
lookupAndInstantiate name table counter = let t = tracedUncondLookup "MilnerTools.lookupAndInstantiate" name table in instantiatedType t counter

tv x = TV $ 't' : show x

-- freshAtoms ������������ ����� � ����� ����� - ���
-- ���������� �������� Definition.loc.argAtoms
freshAtoms :: [String] -> Int -> (Int, [(String, T)])
freshAtoms [] counter = (counter, [])
freshAtoms a counter = (counter + length a, zipWith (\a i -> (a, tv i)) a [counter..])

-- ������������ ��� ������ ���� ��������� � �������� tau
constantType x = case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"

uncurryType p (TT x) = xtrace "unc" $ TT $ map uncurryAll $ uncurryFunctionType x $ xtrace "uncurryType.p" p where
	uncurryFunctionType [argType] [] = [uncurryAll argType]
	uncurryFunctionType argTypes [] = xtrace "xxx" $ map uncurryAll argTypes
	uncurryFunctionType (ht : tt) (_ : ta) = ht : uncurryFunctionType tt ta
	uncurryFunctionType [] _ = error "MilnerTools.uncurryFunctionType encountered []"

	uncurryAll (TT t) = xtrace ("UncurryAll:" ++ show t) $ case last t of
		TT xx -> uncurryAll (TT (init t ++ xx))
	 	_ -> TT t
	uncurryAll x = x

uncurryType _ t = t

type Substitution = M.Map String T

unify :: T -> T -> Substitution
unify a b = xtrace ("unify-trace: " ++ makeType a ++ " ~ " ++ makeType b) d where
	c = myUnify a b
	d = composeSubstitutions c c

closure env t = if S.null varsToSubst then t else mapTypeTU mapper t where
	tpv = typeAllPolyVars t
	epv = xtrace "closure.epv" $ envPolyVars env
	varsToSubst = xtrace "closure.varsToSubst" $ tpv S.\\ epv
	mapper (TU a) = xtrace "closure.mapper!" $ if S.member a varsToSubst then xtrace "Closure.TU" (TU a) else TV a
	envPolyVars = M.fold f S.empty where
		f el acc = S.union acc $ typeAllPolyVars el

normalizeSubstitution a = composeSubstitutions2 a a

composeSubstitutions a b = a `composeSubstitutions2` b `composeSubstitutions2` a

composeSubstitutions2 :: Substitution -> Substitution -> Substitution
composeSubstitutions2 a b | M.null a = b
composeSubstitutions2 a b | M.null b = a

composeSubstitutions2 b a = xtrace ("MilnerTools.composeSubstitutions: " ++ show a ++ " # " ++ show b) $ M.fold composeSubstitutions (a `M.union` b') $ M.intersectionWith unify a b' where
	b' = M.map (`substituteType` a) b

instantiatedType :: T -> Int -> (Int, T)
instantiatedType t counter = (nextCounter, substituteType t substitutions) where
	foo = zip (S.toList $ typeTu t) [counter..]
   	nextCounter = counter + length (S.toList $ typeTu t)
	substitutions = M.fromList $ map (second tv) foo

substituteType t substitutions = xtrace ("stv : " ++ show substitutions ++ " # " ++ show t) $ subst t where
	subst t = case t of
		TU a -> fromMaybe (TU a) (M.lookup a substitutions)
		TV a -> fromMaybe (TV a) (M.lookup a substitutions)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t
