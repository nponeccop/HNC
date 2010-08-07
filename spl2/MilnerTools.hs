module MilnerTools where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import SPL.Top
import Utils
import SPL.Visualise
import Test.HUnit
import Debug.Trace
import HN.Intermediate
import CPP.TypeProducer
import SPL.Types


instantiateLibrary m = M.map (\x -> TLib x) m


freshAtoms :: [String] -> Int -> (Int, [(String, T)])

freshAtoms [] counter = (counter, [])
freshAtoms a counter = (counter + length a, zipWith (\a i -> (a, tv i)) a [counter..])

substituteType :: [Substitution] -> T -> T
substituteType [] b = b
substituteType a b = result where -- trace ("substituteType:" ++ show a ++ " ===> " ++ show result) result where
	result = substituteSingle (composeSubstitutions a) b

substituteSingle a b = substituteTypeVars  b a

ztrace m t =  trace (m ++ " = " ++ show t) t

symTrace m t = trace (m ++ " = " ++ show ( M.difference t SPL.Top.get_types)) t

substitute :: [Substitution] -> M.Map String T -> M.Map String T
substitute [] b = b
substitute a b = xtrace "substitute.result: " $ M.map (substituteType a) b

-- use TRACE, not ZTRACE!
unify :: T -> T -> [Substitution]
unify a b = xtrace ("unify-trace: " ++ makeType a ++ " ~ " ++ makeType b ++ " = " ++ show c) c where
	c = xunify a b
	xunify (TT (a:at @ (_:_))) (TT (b:bt @ (_:_))) = xunify a b `xcompose` xunify (TT at) (TT bt)

 	xunify (TT [a]) b = xunify a b
 	xunify a (TT [b]) = xunify a b

	xunify (TD a a1) (TD b b1) | a == b = concat $ zipWith xunify a1 b1
	xunify (T a) (T b) | a == b = []
	xunify (TU a) b = [M.singleton a b]
	xunify b (TU a) = [M.singleton a b]
	xunify a b = error $ "unify: " ++ show a ++ " ~ " ++ show b

closure _ b = TLib b -- temporary! must be implememted properly to support "local" polymorphism

replace k v m = M.insert k v m

-- Program.s.tau:
-- Program.s: plusX, where where2 locals14 locals4 false false2 print15 euler1 euler6 euler48 euler5
-- Program.s: locals6-1 locals6 locals7 test4 locals13 test_flip3a locals12 intfunc_simple locals11 test_flip3 io2_intfunc udp_echo_server2 flip4_hn
-- compose: natrec, polyfunc_pointer
-- loop: intfunc test2 e48 t2_identifier elist


lookupAndInstantiate :: String -> M.Map String T -> Int -> (Int, T)
lookupAndInstantiate name table counter = let t = uncondLookup name table in case t of
	TLib x -> instantiatedType x counter
	_ -> (counter, t)

tv x = TU $ "t" ++ show x

constantType x = case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"

lookupAtom name visibleAtoms freshVar = case M.lookup name visibleAtoms of
	Nothing -> error "foo" -- $ (freshVar + 1, tv freshVar)
	Just t -> error "lookupAtom" -- instantiatedType freshVar t


xtrace a b = b

xcompose :: [Substitution] -> [Substitution] -> [Substitution]
xcompose [a] [b] = [M.fromList $ xcompose2 (M.toList a) (M.toList b)]
xcompose a b = xtrace ("xcompose-old!!!: " ++ show a ++  " o " ++ show b) $ a ++ b

xxunify a b = M.toList $ composeSubstitutions $ unify a b

xcompose2 :: [(String, T)] -> [(String, T)] -> [(String, T)]
xcompose2 [(name1, val1)] [(name2, val2)] | name1 == name2
	= [(name1, val1)] ++ xxunify val1 val2

xcompose2 a b = xtrace "xcompose2-old!!!" $ a ++ b

instantiatedTypeTest t e = TestLabel "instantiatedTypeTest" $ TestCase $ assertEqual "" e  $ makeType $ snd $ instantiatedType (libType t) 10

instantiatedTypeTests = [
		instantiatedTypeTest "print" "?t10 -> IO void"
	,	instantiatedTypeTest "bind" "IO ?t10 -> (?t10 -> IO ?t11) -> IO ?t11"
	]

libType name = uncondLookup name SPL.Top.get_types

instantiatedType :: T -> Int -> (Int, T)
instantiatedType t counter = (nextCounter, substituteTypeVars t substitutions) where
	foo = zip (S.toList $ typePolyVars t) [counter..]
   	nextCounter = counter + (length $ S.toList $ typePolyVars t)
	substitutions = M.fromList $ map (\(x,y) -> (x, tv y)) foo

composeSubstitutions a = xtrace ("composeSubstitutions: " ++ show a ++ " ====> " ++ show b) b where
	b = foldr compose M.empty a


compose x y  = M.fromList $ map m1 $ M.toList u where
	u = M.union x y
	m1 :: (String, T) -> (String, T)
	m1 (k, v) = (k, xsubst v u)
	xsubst :: T -> M.Map String T -> T
	xsubst v x = substituteTypeVars v x

substituteTypeVars t substitutions = subst t where
	subst t = case t of
		TU a -> (case M.lookup a substitutions of
			Nothing -> TU a
			Just b -> b)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t

type Substitution = M.Map String T
