module MilnerTools where
import qualified Data.Map as M
import qualified Data.Set as S
import SPL.Top
import Utils
import SPL.Visualise
import Test.HUnit
import Debug.Trace
import HN.Intermediate
import CPP.TypeProducer
import SPL.Types


freshAtoms :: [String] -> Int -> (Int, [(String, T)])

freshAtoms [] counter = (counter, [])
freshAtoms a counter = (counter + length a, zipWith (\a i -> (a, tv i)) a [counter..])

symTrace m t = trace (m ++ " = " ++ show ( M.difference t SPL.Top.get_types)) t

substitute :: Substitution -> M.Map String T -> M.Map String T
substitute a b = xtrace "substitute.result: " $ M.map (\x -> substituteTypeVars x a) b

unify :: T -> T -> [Substitution]
unify a b = xtrace ("unify-trace: " ++ makeType a ++ " ~ " ++ makeType b) c where
	c = xunify a b
	xunify (TT (a:at @ (_:_))) (TT (b:bt @ (_:_))) = [xunify a b `xcompose` xunify (TT at) (TT bt)]

 	xunify (TT [a]) b = xunify a b
 	xunify a (TT [b]) = xunify a b

	xunify (TD a a1) (TD b b1) | a == b = [composeSubstitutions $ concat $ zipWith xunify a1 b1]
	xunify (T a) (T b) | a == b = []
	xunify (TU a) (TU b) | a == b = []
	xunify (TU a) (TV b) | a == b = []

	xunify (TU a) b = [M.singleton a b]
	xunify b (TU a) = [M.singleton a b]
	xunify (TV a) b = xunify (TU a) b
	xunify b (TV a) = xunify (TU a) b
	xunify a b = error $ "cannot unify: " ++ show a ++ " ~ " ++ show b

envPolyVars e = M.fold f S.empty e where
	f el acc = S.union acc $ typeAllPolyVars el

mapTypeTU f t = subst t where
	subst t = case t of
		TU a -> f (TU a)
		TV a -> f (TU a)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t


closure env t = if S.null varsToSubst then t else mapTypeTU mapper t where
	tpv = typeAllPolyVars t
	epv = xtrace "closure.epv" $ envPolyVars env
	varsToSubst = xtrace "closure.varsToSubst" $ tpv `subtractSet` epv
	mapper (TU a) = xtrace "closure.mapper!" $ if S.member a varsToSubst then xtrace "Closure.TU" (TU a) else TV a

lookupAndInstantiate :: String -> M.Map String T -> Int -> (Int, T)
lookupAndInstantiate name table counter = let t = uncondLookup name table in instantiatedType t counter

tv x = TV $ "t" ++ show x

constantType x = case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"

xcompose :: [Substitution] -> [Substitution] -> Substitution
xcompose a b = composeSubstitutions (a ++ b)

xcompose2 :: Substitution -> Substitution -> Substitution
xcompose2 a b | M.null a = b
xcompose2 a b | M.null b = a


xcompose2 a b = xtrace ("MilnerTools.xcompose2: " ++ show a ++ " # " ++ show b) $ M.fold xcompose2 (M.union a b') $ M.intersectionWith (\a b -> composeSubstitutions $ unify a b) a b' where
	b' = M.map (\x -> substituteTypeVars x a) b

substituteType :: [Substitution] -> T -> T
substituteType a b = substituteTypeVars b $ composeSubstitutions a


composeSubstitutions a = xtrace ("composeSubstitutions: " ++ show a ++ " ====> " ++ show b) b where
	b = foldr xcompose2 M.empty a

instantiatedTypeTest t e = TestLabel "instantiatedTypeTest" $ TestCase $ assertEqual "" e  $ makeType $ snd $ instantiatedType (libType t) 10

instantiatedTypeTests = [
		instantiatedTypeTest "print" "?t10 -> IO void"
	,	instantiatedTypeTest "bind" "IO ?t10 -> (?t10 -> IO ?t11) -> IO ?t11"
	]

libType name = uncondLookup name SPL.Top.get_types

instantiatedType :: T -> Int -> (Int, T)
instantiatedType t counter = (nextCounter, substituteTypeVarsNoTv t substitutions) where
	foo = zip (S.toList $ typePolyVars t) [counter..]
   	nextCounter = counter + (length $ S.toList $ typePolyVars t)
	substitutions = M.fromList $ map (\(x,y) -> (x, tv y)) foo


substituteTypeVarsNoTv t substitutions | M.null substitutions = t
substituteTypeVarsNoTv t substitutions = xtrace "stvnoTv" $ subst t where
	subst t = case t of
		TU a -> (case M.lookup a substitutions of
			Nothing -> TU a
			Just b -> b)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t

substituteTypeVars t substitutions = xtrace ("stv : " ++ show substitutions ++ " # " ++ show t) $ subst t where
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

uncurryAll (TT t) = xtrace ("UncurryAll:" ++ show t) $ case last t of
	TT xx -> uncurryAll (TT (init t ++ xx))
 	_ -> TT t
uncurryAll x = x

uncurryType p (TT x) = TT $ map uncurryAll $ uncurryFunctionType x $ xtrace "uncurryType.p" p where
	uncurryFunctionType [argType] [] = [uncurryAll argType]
	uncurryFunctionType argTypes [] = [TT $ map uncurryAll argTypes]
	uncurryFunctionType (ht : tt) (_ : ta) = ht : uncurryFunctionType tt ta
	uncurryFunctionType [] _ = error "MilnerTools.uncurryFunctionType encountered []"

uncurryType _ t = t

type Substitution = M.Map String T
