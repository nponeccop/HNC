
module Check3 (P (..), check0, check, res) where

import Data.Map as M hiding (filter, union)

import Types
import Code hiding (res)
import Top
import Debug.Trace
--import Hugs.Observe
--observe a b = b
trace2 a b = trace ("<\n"++a++"\n  "++show b++"\n>") b
observeN a b = b

data P = P (Map [Char] T, T) | N [Char]
	deriving Show


get_r (P (ur, r)) = r
get_rl l = Prelude.map get_r l
get_url [] =
	Right []
get_url ((P (ur, r)):rs) =
	case get_url rs of
		Right o -> Right (ur:o)
		Left o -> Left o
get_url ((N o):rs) =
	Left (N o)

union_r a b =
	let m = M.intersectionWith (\a b -> (a, b, Check3.compare a b)) a b in
	let (r, m2) = M.mapAccum (\z (a,b,(l,r,_)) -> (union_r z r, merge a b)) M.empty m in
		M.unionsWith merge [a, b, r]

union a b =
	let m = M.intersectionWith (\a b -> (a, b, Check3.compare a b)) a b in
	let (r, m2) = M.mapAccum (\z (a,b,(l,r,_)) -> (union z (union l M.empty), merge a b)) M.empty m in
		M.unionsWith merge [a, b, r]

merge (TT a) (TT b)|length a == length b = TT $ zipWith merge a b
merge (TD n a) (TD n2 b)|n==n2 && length a==length b = TD n $ zipWith merge a b
merge (TU a) (TU b) = TU a
merge (TU a) b = observeN ("merge1"++show a) b
merge a (TU b) = observeN ("merge2"++show b) a
merge (TV a) (TV b) = TV a
merge (TV a) b = b
merge a (TV b) = a
merge TL TL = TL
merge (T n) (T n2)|n==n2 = T n
merge a b = error ("merge: {"++show a++", "++show b++"}")

get_ul n u =
	case M.lookup n u of
		Just n -> show n
		Nothing -> "<nil>"

ch [] [] et ul uv i sv =
	N "too many parameters"
ch (r:[]) [] et ul uv i sv =
	P (observeN "uv" uv, setmv (setm (observeN "r" r) (observeN "l" ul)) uv)
ch r [] et ul uv i sv =
	P (uv, setmv (setm (TT r) ul) uv)
ch (r:rs) (p1:ps) et ul uv i sv =
	case observeN ("ch_p "++show p1) $ check p1 et sv of
		P (rm, r_p1) ->
			let r_p2 = change_tu (observeN "r_p1" r_p1) i in
			let rm2 = M.map (\x -> change_tu x i) (observeN "rm" rm) in
			case Check3.compare (observeN ("cmp1") (setmv (setm r ul) rm2)) (observeN "cmp2" r_p2) of
				(l2, r2, True) ->
					let ru1u = union_r uv rm2;
							ru2u = union_r r2 rm2;
							ru3u = union_r r2 uv;
							ru1 = observeN "ru1" $ M.map (\x -> setm (setm x ru1u) lu) r2;
							ru2 = observeN "ru2" $ M.map (\x -> setm (setm x ru2u) lu) uv;
							ru3 = observeN "ru3" $ M.map (\x -> setm (setm x ru3u) lu) rm2;
							ru = observeN "ru" $ union_r (union_r ru1 ru2) ru3;
							lu1 = M.map (\x -> setm (setm x ul) ru) l2;
							lu2 = M.map (\x -> setm (setm x l2) ru) ul;
							lu = observeN "lu" $ union lu1 lu2
					in ch rs ps et lu ru (i+(1::Int)) sv
				(l2, r2, False) ->
					N ("expected "++show (setm r ul)++", actual "++show r_p1)
		N e -> N e

check::C -> Map [Char] T -> [[Char]] -> P
check (CNum n) et _ = P (M.empty, T "num")
check (CBool n) et _ = P (M.empty, T "boolean")
check (CStr n) et _ = P (M.empty, T "string")
check (CVal n) et _ =
	case M.lookup n et of
		Just a -> P (M.empty, a)
		Nothing -> N $ (++) "check cannot find " $ show n

check (CL a (K [])) et sv =
	check a et sv

check (CL a (K p)) et sv =
	observeN ("K:"++show a++" |"++show p) $
	case check a et sv of
		P (rm0, TT r) ->
			case ch r p et M.empty rm0 0 sv of
				P (rm, r) ->
					P (rm, r)
				N e -> N (e++" for "++show a)
--		P (rm, TU n) ->
--			P (putp [n] [TT ((get_rl p_ok)++[TU ('_':n)])] rm, TU ('_':n))
--		P (_, TT []) ->
--			N ("too many parameters for "++show a)
--		P (ur, TU n) ->
--			P (putp [n] [TT (get_rl p_ok++[TU ('_':n)])] M.empty, TU ('_':n)) -- ?
		P (ur, TV n) ->
				case get_url p_ok of
					Right a -> 
						let rm = observeN "rm" $ putp [n] [TT (get_rl p_ok++[TU ('_':n)])] $ foldr (\a b -> union_r a b) M.empty a;
							r = observeN "r" $ TU ('_':n)
						in P (union_r (observeN ("rm"++show rm) rm) ur, setm r rm)
					Left o -> o
		N e -> N e
	where
		p_ok = Prelude.map (\x -> check x et sv) p

check (CL a (S [])) et sv =
	observeN "S" $ check a et sv

check (CL a (S (p:ps))) et sv =
	case check (CL a (S ps)) (putp [p] [TV p_n] et) sv of
		P (ur, r) ->
			case M.lookup (p_n) ur of
				Just v ->
					let w = case (v, r) of
						(a, TT b) -> TT (a:b)
						(a, TV n) -> TT [a, TU n]
						(a, b) -> TT [a, b]
					in
					let ur2 = case elem p_n sv of
						True -> ur
						False -> M.delete p_n ur
					in
					observeN ("ok "++p++"|"++show a) $ P (ur2, untv p w)
				Nothing ->
					let w = case r of
						TT b -> TT ((TU p_n):b)
						TV n -> TT [TU p_n, TU n]
						b -> TT [TU p_n, b]
					in
					observeN ("no "++p) $ P (M.map (untv p) ur, w) -- rm ?
		o -> o
	where p_n = ""++p

check (CL a L) et sv =
	observeN ("L:"++show a) $
	case check a et sv of
		P (ur, r) ->
			P (ur, TT [TL, r])
		o -> o
	
check (CL a R) et sv =
	case check a (putp ["_f"] [TV "_f"] et) sv of
		P (ur, r) -> check a (putp ["_f"] [r] et) sv
		o -> o

check o et sv =
	error ("check o: "++show o)

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et
putp o1 o2 et = error ("Check3.putp: "++show o1++", "++show o2)

compare (T a) (T b)|a == b = (M.empty, M.empty, True)
compare (TD a l1) (TD b l2)|a == b = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (union u1l u2l, union_r u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check3.compare l1 l2
--compare (TT l1) (TT l2) = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check3.compare l1 l2
-- error: TT [T "num",TU "_l"]/TT [TU "_",TU "z",T "num"]
compare (TT []) (TT []) =
	(M.empty, M.empty, True)
compare (TT [TU a]) b@(TT l)|1 < length l =
	(M.singleton a b, M.empty, True)
compare (TT (l1:l1s)) (TT (l2:l2s)) =
	(union l ll, union_r r rr, b && bb)
	where
		(l, r, b) = Check3.compare l1 l2
		(ll, rr, bb) = Check3.compare (TT l1s) (TT l2s)
compare (TU a) (TV b) = (M.singleton a (TV b), M.singleton b (TU a), True)
compare a (TV n) = (M.empty, M.singleton n a, True)
compare (TV n) b = (M.empty, M.singleton n b, True)
--compare (TU a) (TU b) = (M.singleton a (TU b), M.empty, True)
compare (TU a) (TU b) = (M.unionWith merge (M.singleton b (TU a)) (M.singleton a (TU b)), M.empty, True)
compare (TU n) b = (M.singleton n b, M.empty, True)
compare a (TU n) = (M.singleton n a, M.empty, True) -- correct ?
compare TL TL = (M.empty, M.empty, True) -- return lazy?
--compare t1 t2 = error $ (show t1)++"/"++(show t2)
compare t1 t2 = (M.empty, M.empty, False)

setu (TD n tt) u = TD n (Prelude.map (\t -> setu t u) tt)
setu (TT tt) u = TT (Prelude.map (\t -> setu t u) tt)
setu (TU n) (t2:t2s) = t2
setu o (t2:t2s) = o
setu o [] = o

setml l u = Prelude.map (\x -> setm x u) l
setm (TD n tt) u = TD n (Prelude.map (\t -> setm t u) tt)
setm (TT tt) u = TT (Prelude.map (\t -> setm t u) tt)
setm (TU n) u =
	case M.lookup n u of
		Just a -> a
		Nothing -> TU n
setm o u = o

setmvl l u = Prelude.map (\x -> setmv x u) l
setmv (TD n tt) u = TD n (Prelude.map (\t -> setmv t u) tt)
setmv (TT tt) u = TT (Prelude.map (\t -> setmv t u) tt)
setmv (TV n) u =
	case M.lookup n u of
		Just a -> a
		Nothing -> TV n
setmv o u = o

untv p (TD n tt) = TD n (Prelude.map (\t -> untv p t) tt)
untv p (TT tt) = TT (Prelude.map (\t -> untv p t) tt)
untv p (TV n)|p == n = TU n
untv p (TV n) = TV n
untv p o = o

change_tul tt i = Prelude.map (\t -> change_tu t i) tt
change_tu (TT tt) i = TT $ change_tul tt i
change_tu (TD n tt) i = TD n $ change_tul tt i
change_tu (TU n) i = TU (n++show i)
change_tu o i = o

change_tvl tt i = Prelude.map (\t -> change_tv t i) tt
change_tv (TT tt) i = TT $ change_tvl tt i
change_tv (TD n tt) i = TD n $ change_tvl tt i
change_tv (TV n) i = TV (n++show i)
change_tv o i = o

check0 o =
	observeN "ret" $ check o Top.get_types []

-- (_*sum (length _) (head _))
res = check (CL (CL (CL (CL (CVal "hd") (K [CVal "sum",CNum 5])) (S ["hd"])) (K [CL (CL (CVal "z") (K [CVal "a",CVal "x"])) (S ["z","a"])])) (S ["x"])) Top.get_types ["hd"]



