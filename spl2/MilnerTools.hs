module MilnerTools where
import qualified Data.Map as M
import qualified Data.Set as S
import Utils
import SPL.Visualise
import HN.Intermediate
import SPL.Types

lookupAndInstantiate :: String -> M.Map String T -> Int -> (Int, T)
lookupAndInstantiate name table counter = let t = uncondLookup name table in instantiatedType t counter

tv x = TV $ "t" ++ show x

-- freshAtoms используется всего в одном месте - при
-- вычислении атрибута Definition.loc.argAtoms
freshAtoms :: [String] -> Int -> (Int, [(String, T)])
freshAtoms [] counter = (counter, [])
freshAtoms a counter = (counter + length a, zipWith (\a i -> (a, tv i)) a [counter..])

-- используется при выводе типа константы в качестве tau
constantType x = case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"

uncurryType p (TT x) = TT $ map uncurryAll $ uncurryFunctionType x $ xtrace "uncurryType.p" p where
	uncurryFunctionType [argType] [] = [uncurryAll argType]
	uncurryFunctionType argTypes [] = [TT $ map uncurryAll argTypes]
	uncurryFunctionType (ht : tt) (_ : ta) = ht : uncurryFunctionType tt ta
	uncurryFunctionType [] _ = error "MilnerTools.uncurryFunctionType encountered []"

	uncurryAll (TT t) = xtrace ("UncurryAll:" ++ show t) $ case last t of
		TT xx -> uncurryAll (TT (init t ++ xx))
	 	_ -> TT t
	uncurryAll x = x

uncurryType _ t = t

type Substitution = M.Map String T

substituteEnv :: Substitution -> M.Map String T -> M.Map String T
substituteEnv a b = xtrace "substitute.result: " $ M.map (\x -> substituteType x a) b

unify :: T -> T -> Substitution
unify a b = xtrace ("unify-trace: " ++ makeType a ++ " ~ " ++ makeType b) c where
	c = xunify a b
	xunify :: T -> T -> Substitution
	xunify (TT (a:at @ (_:_))) (TT (b:bt @ (_:_))) = xunify a b `composeSubstitutions` xunify (TT at) (TT bt)

 	xunify (TT [a]) b = xunify a b
 	xunify a (TT [b]) = xunify a b

	xunify (TD a a1) (TD b b1) | a == b = foldr unifyAndCompose M.empty $ zip a1 b1 where
		unifyAndCompose (a, b) m = composeSubstitutions m $ unify a b
	xunify (T a) (T b) | a == b = M.empty
	xunify (TU a) (TU b) | a == b = M.empty
	xunify (TU a) (TV b) | a == b = M.empty

	xunify (TU a) b = M.singleton a b
	xunify b (TU a) = M.singleton a b
	xunify (TV a) b = xunify (TU a) b
	xunify b (TV a) = xunify (TU a) b
	xunify a b = error $ "cannot unify: " ++ show a ++ " ~ " ++ show b

closure env t = if S.null varsToSubst then t else mapTypeTU mapper t where
	tpv = typeAllPolyVars t
	epv = xtrace "closure.epv" $ envPolyVars env
	varsToSubst = xtrace "closure.varsToSubst" $ tpv S.\\ epv
	mapper (TU a) = xtrace "closure.mapper!" $ if S.member a varsToSubst then xtrace "Closure.TU" (TU a) else TV a
	envPolyVars e = M.fold f S.empty e where
		f el acc = S.union acc $ typeAllPolyVars el

composeSubstitutions :: Substitution -> Substitution -> Substitution
composeSubstitutions a b | M.null a = b
composeSubstitutions a b | M.null b = a

composeSubstitutions a b = xtrace ("MilnerTools.composeSubstitutions: " ++ show a ++ " # " ++ show b) $ M.fold composeSubstitutions (M.union a b') $ M.intersectionWith unify a b' where
	b' = M.map (\x -> substituteType x a) b

instantiatedType :: T -> Int -> (Int, T)
instantiatedType t counter = (nextCounter, substituteType t substitutions) where
	foo = zip (S.toList $ typeTu t) [counter..]
   	nextCounter = counter + (length $ S.toList $ typeTu t)
	substitutions = M.fromList $ map (\(x,y) -> (x, tv y)) foo

substituteType t substitutions = xtrace ("stv : " ++ show substitutions ++ " # " ++ show t) $ subst t where
	subst t = case t of
		TU a -> (case M.lookup a substitutions of
			Nothing -> TU a
			Just b -> b)
		TV a -> (case M.lookup a substitutions of
			Nothing -> TV a
			Just b -> b)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t
---
--

typeTu x = let union = S.unions . map typeTu in case x of
	TU v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty

typeAllPolyVars x = let union = S.unions . map typeAllPolyVars in case x of
	TU v -> S.singleton v
	TV v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty

typeTv x = let union = S.unions . map typeTv in case x of
		TV v -> S.singleton v
		TT l -> union l
		TD _ l -> union l
		_    -> S.empty

mapTypeTU f t = subst t where
	subst t = case t of
		TU a -> f (TU a)
		TV a -> f (TU a)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t
