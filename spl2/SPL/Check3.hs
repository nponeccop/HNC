
module SPL.Check3 (P (..), check, check_with_rename, res) where

import System.IO.Unsafe
import qualified Data.Map as M

import SPL.Types
import qualified SPL.Parser
import SPL.Compiler hiding (res)
import Debug.Trace
observeN a b = b

observe s v = trace ("{"++s++":"++show v++"}") v
--observe s v = v

data P = P ([Char], M.Map [Char] T, T) | N Int [Char]
	deriving Show


get_r (P (_, ur, r)) = r
get_rl l = Prelude.map get_r l
get_url [] =
	Right []
get_url ((P (_, ur, r)):rs) =
	case get_url rs of
		Right o -> Right (ur:o)
		Left o -> Left o
get_url ((N i o):rs) =
	Left (N i o)

union_r a b =
	let a2 = M.filter (\a -> case a of TDebug _ -> False; _ -> True) a in
	let b2 = M.filter (\a -> case a of TDebug _ -> False; _ -> True) b in
	let m = M.intersectionWith (\a b -> (a, b, SPL.Check3.compare a b)) a2 b2 in
	let (r, m2) = M.mapAccum (\z (a,b,(l,r,_)) -> (union_r z r, merge a b)) M.empty m in
		M.unionsWith merge [a, b, r]

union a b =
	let m = M.intersectionWith (\a b -> (a, b, SPL.Check3.compare a b)) a b in
	let (l, m2) = M.mapAccum (\z (a,b,(l,r,_)) -> (union z l, merge a b)) M.empty m in
		M.unionsWith merge [a, b, l]

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
merge (TUL a) b = TUL (b:a)
merge a b = TUL [b, a]
--merge a b = error ("merge: {"++show a++", "++show b++"}")

get_ul n u =
	case M.lookup n u of
		Just n -> show n
		Nothing -> "<nil>"

setmm a b = M.map (\x -> setm x b) a

ch [] [] et ul uv i sv ii =
	N ii "too many parameters"
ch (r:[]) [] et ul uv i sv ii =
	let z = setmv (setm (observeN "r" r) (observeN "ul" ul)) $ observeN "uv" uv in
	P ("", observeN "uv" $ M.map untv_all uv, z)
ch r [] et ul uv i sv ii =
	P ("", uv, setmv (setm (TT r) ul) uv)
ch (r:rs) (p1:ps) et ul uv i sv ii =
	case observeN ("ch_p "++show r) $ check p1 et sv of
		P (_, rm, r_p1) ->
			let r_p2 = change_tu (observeN "r_p1" r_p1) "" i in
			let rm2 = M.map (\x -> change_tu x "" i) (observeN "rm" rm) in
			case SPL.Check3.compare (observeN "cmp1" (setmv (setm r ul) rm2)) (observeN "cmp2" r_p2) of
				(l2, r2, True) ->
					let rr = union_r r2 $ union_r rm2 (observeN "uv" uv) in
					let ll = union l2 ul in
					let rrr = setmm rr rr in
					let lll = setmm ll ll in
					let ru = setmm rrr lll in
					let lu = setmm lll rrr in
						ch rs ps et lu ru (i+(1::Int)) sv ii
				(l2, r2, False) ->
					let iii = case p1 of
						CDebug i _ -> i
						o -> ii
					in
					N iii ("expected "++show (setm r ul)++", actual "++show r_p1)
		N i e -> N i e

check::C -> M.Map [Char] T -> Bool -> P
--check (CF n) et _ = P (M.empty, T "-z_")
check (CNum n) et _ = P ("", M.empty, T "num")
check (CBool n) et _ = P ("", M.empty, T "boolean")
check (CStr n) et _ = P ("", M.empty, T "string")
check (CDebug i (CVal n)) et _ =
	case M.lookup n et of
		Just a ->
			case a of
				TV a -> P ("", M.empty, TV a)
				o -> P ("", M.empty, o)
		Nothing -> N i ("check cannot find "++show n)
check (CVal n) et sv =
	check (CDebug (-1) (CVal n)) et sv

check (CStruct m) et sv =
	P ("", M.empty, TS $ M.map (\x -> get_r $ check x et sv) m)

--check (CDot (CStruct m) n) et sv =
--	case M.lookup n m of
--		Just a -> check a et sv
--		Nothing -> error "err100"

{-check (CDebug i (CDot a n)) et sv =
	case check a et sv of
		P (_, TS m) ->
			case M.lookup n m of
				Just a -> P (M.empty, a)
				Nothing -> N i ("field is not correct for the structure")
		P (_, TV n2) -> P (M.singleton n2 (TS $ M.singleton n (TU (n2++"."++n))), TU n)
		N i o -> N i o

check (CDot a n) et sv =
	check (CDebug (-1) (CDot a n)) et sv-}

check (CL a (K [])) et sv =
	check a et sv

check (CDebug ii (CL (CDebug _ (CVal "load")) (K ((CDebug _ (CStr f)):[])))) et sv =
	unsafePerformIO $
	do
		str <- readFile f
		return $ case SPL.Parser.parse str of
			SPL.Parser.P _ i p ->
				check (compile p) et sv
			SPL.Parser.N i -> N i "check load error"

check (CDebug ii (CL a (K p))) et sv =
	case check (observeN "a" a) et sv of
		P (_, rm0, TT r) ->
			case ch (observeN ("r_"++show a) r) p et M.empty (observeN "rm0" rm0) 0 sv ii of
				P (_, rm, r) -> P ("", observeN "rm" rm, observeN "r" r)
				N i e -> N i e
		P (_, ur, TV n) ->
				case get_url p_ok of
					Right a -> 
						let rm = observeN "rm" $ putp [n] [TT (get_rl p_ok++[TU ('_':n)])] $ foldr (\a b -> union_r a b) M.empty a;
							r = observeN "r" $ TU ('_':n)
						in P ("", union_r (observeN ("rm"++show rm) rm) ur, setm r rm)
					Left o -> o
		P (_, ur, TU n) ->
				case get_url p_ok of
					Right a -> 
						let rm = observeN "rm" $ putp [n] [TT (get_rl p_ok++[TU ('_':n)])] $ foldr (\a b -> union_r a b) M.empty a;
							r = observeN "r" $ TU ('_':n)
						in P ("", M.map (\x -> norm $ setm x rm) ur, setm r rm)
					Left o -> o
		N i e -> N i e
	where
		p_ok = Prelude.map (\x -> check x et sv) p
		norm (TT o) =
		  case reverse o of
		    (TT a):b -> norm $ TT $ (reverse b)++a
		    _ -> TT o
		norm o = o

check (CL a (K p)) et sv =
	check (CDebug (-1) (CL a (K p))) et sv

check (CL a (S [])) et sv =
	observeN "S" $ check a et sv

check (CL a (S (p:ps))) et sv =
	case check (CL a (S ps)) (putp [p] [TV p_n] et) sv of
		P ("", ur, r) ->
			case M.lookup (p_n) (observeN "ur" ur) of
				Just v ->
					let w = case (v, r) of
						((TDebug a), b) -> TT [TU p_n, b]
						(a, TT b) -> TT (a:b)
						(a, TV n) -> TT [a, TU n]
						(a, b) -> TT [a, b]
					in
					let ur2 = case sv of
						True -> M.map (\x -> TDebug $ untv p_n x) ur
						False -> M.map (untv p_n) $ M.delete p_n ur
					in
					observeN ("ok "++p++"|"++show a) $ P ("yes", observeN "ur2" ur2, untv p_n w)
				Nothing ->
					let w = case r of
						TT b -> TT ((TU p_n):b)
						TV n -> TT [TU p_n, TU n]
						b -> TT [TU p_n, b]
					in
					let ur2 = case sv of
						True -> M.insert p_n (TDebug $ TU p_n) $ M.map (untv p_n) ur
						False -> M.map (untv p_n) ur
					in
					observeN ("no "++p) $ P ("no", ur2, w) -- rm ?
		o -> o
	where p_n = ""++p

--check (CL (CDebug _ (CStruct m)) (W ws)) et sv|M.null m =
check (CL (CStruct m) (W ws)) et sv|M.null m =
	ch_struct (TS M.empty) ws et sv

check (CL a (W [])) et sv =
	check a et sv

check (CL a (W ((n, p):ws))) et sv =
	check (CL (CL (CL a (W ws)) (S [n])) (K [p])) et sv

check (CL a (D n)) et sv =
	check (CDebug (-1) (CL a (D n))) et sv

check (CDebug i (CL a (D n))) et sv =
	case check a et sv of
		P ("", _, TS m) ->
			case M.lookup n m of
				Just a -> P ("", M.empty, a)
				Nothing -> N i ("field did not find: "++n)
		P (_, _, TV n2) -> P ("", M.singleton n2 (TS $ M.singleton n (TU (n2++"."++n))), TU n)
		P (_, _, TU n2) -> P ("", M.empty, TU n)
		N i o -> N i o

check (CL a L) et sv =
	observeN ("L:"++show a) $
	case check a et sv of
		P (_, ur, r) ->
			P ("", ur, TT [TL, r])
		o -> o
	
check (CL a R) et sv =
	case check a (putp ["_f"] [TV "_f"] et) sv of
		P (_, ur, r) ->
			case check a (putp ["_f"] [r] et) sv of
				P (_, ur2, r2) ->
					case M.lookup "_f" ur of
						Just v -> P ("", ur2, merge v r)
						Nothing -> error "_f"
				o -> o
		o -> o

check (CDebug _ c) et sv =
	check c et sv

check (CInFun _ (InFun n f)) et sv =
	P ("", M.empty, ((read n)::SPL.Types.T))

check o et sv =
	error ("check o: "++show o)

ch_struct (TS m) [] et sv =
	P ("", M.empty, TS m)

ch_struct (TS m) ((n,p):ws) et sv =
	case check p et sv of
		P (_, _, r) ->
			ch_struct (TS $ M.insert n r m) ws (putp [n] [r] et) sv
		N i o -> N i o

putp (v:vs) (c:cs) et = putp vs cs (M.insert v c et)
putp [] [] et = et
putp o1 o2 et = error ("Check3.putp: "++show o1++", "++show o2)

compare (T a) (T b)|a == b = (M.empty, M.empty, True)
compare (TD a l1) (TD b l2)|a == b = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (union u1l u2l, union_r u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith SPL.Check3.compare l1 l2
--compare (TT l1) (TT l2) = foldr (\(u1l,u1r,r1) (u2l,u2r,r2) -> (M.union u1l u2l, M.union u1r u2r, r1 && r2)) (M.empty, M.empty, True) $ zipWith Check3.compare l1 l2
-- error: TT [T "num",TU "_l"]/TT [TU "_",TU "z",T "num"]
compare (TT []) (TT []) =
	(M.empty, M.empty, True)
compare (TT [TU a]) b@(TT l)|1 < length l =
	(M.singleton a b, M.empty, True)
compare (TT (l1:l1s)) (TT (l2:l2s)) =
	(union l ll, union_r r rr, b && bb)
	where
		(l, r, b) = SPL.Check3.compare l1 l2
		(ll, rr, bb) = SPL.Check3.compare (TT l1s) (TT l2s)
compare (TUL []) b =
	(M.empty, M.empty, False)
compare (TUL (l1:l1s)) l2 =
	(union l ll, union_r r rr, b || bb)
	where
		(l, r, b) = SPL.Check3.compare l1 l2
		(ll, rr, bb) = SPL.Check3.compare (TT l1s) l2
compare (TU a) (TV b) = (M.singleton a (TV b), M.singleton b (TU a), True)
compare a (TV n) = (M.empty, M.singleton n a, True)
compare (TV n) b = (M.empty, M.singleton n b, True)
--compare (TU a) (TU b) = (M.singleton a (TU b), M.empty, True)
compare (TU a) (TU b) = (union {-(M.singleton b (TU a))-} M.empty (M.singleton a (TU b)), M.empty, True)
compare (TU n) b = (M.singleton n b, M.empty, True)
compare a (TU n) = (M.singleton n a, M.empty, True) -- correct ?
compare (TS m1) (TS m2) =
	foldr
		(\(u1l,u1r,r1) (u2l,u2r,r2) -> (union u1l u2l, union_r u1r u2r, r1 && r2))
		(M.empty, M.empty, True)
	$ Prelude.map (\x ->
		case (M.lookup x m1, M.lookup x m2) of
			((Just a), Just b) -> SPL.Check3.compare a b
			((Just a), Nothing) -> (M.empty, M.empty, False)
	) $ M.keys m1


compare TL TL = (M.empty, M.empty, True) -- return lazy?
--compare t1 t2 = error $ (show t1)++"/"++(show t2)
compare (TDebug a) b = error ("d1:"++show a++"/"++show b) -- SPL.Check3.compare a b
compare a (TDebug b) = error "d2" -- SPL.Check3.compare a b
compare t1 t2 = (M.empty, M.empty, False)

setml l u = Prelude.map (\x -> setm x u) l
setm (TD n tt) u = TD n (Prelude.map (\t -> setm t u) tt)
setm (TT tt) u = TT (Prelude.map (\t -> setm t u) tt)
setm (TS m) u = TS $ M.map (\t -> setm t u) m
setm (TU n) u =
	case M.lookup n u of
		Just (TDebug a) -> TU n
		Just a -> a
		Nothing -> TU n
setm (TDebug n) u = TDebug $ setm n u
setm o u = o

setmvl l u = Prelude.map (\x -> setmv x u) l
setmv (TD n tt) u = TD n (Prelude.map (\t -> setmv t u) tt)
setmv (TT tt) u = TT (Prelude.map (\t -> setmv t u) tt)
setmv (TS m) u = TS $ M.map (\t -> setmv t u) m
setmv (TV n) u =
	case M.lookup n u of
		Just (TDebug a) -> TV n
		Just a -> a
		Nothing -> TV n
setmv (TDebug n) u = TDebug $ setmv n u
setmv o u = o

untv p (TD n tt) = TD n (Prelude.map (\t -> untv p t) tt)
untv p (TT tt) = TT (Prelude.map (\t -> untv p t) tt)
untv p (TS tt) = TS $ M.map (\x -> untv p x) tt
untv p (TV n)|p == n = TU n
untv p (TV n) = TV n
untv p (TDebug n) = TDebug $ untv p n
untv p o = o
untv_all (TD n tt) = TD n (Prelude.map (\t -> untv_all t) tt)
untv_all (TT tt) = TT (Prelude.map (\t -> untv_all t) tt)
untv_all (TS tt) = TS $ M.map (\x -> untv_all x) tt
untv_all (TV n) = TU n
untv_all (TDebug n) = TDebug $ untv_all n
untv_all o = o

change_tul tt p i = Prelude.map (\t -> change_tu t p i) tt
change_tu (TT tt) p i = TT $ change_tul tt p i
change_tu (TD n tt) p i = TD n $ change_tul tt p i
change_tu (TU n) p i = TU (n++p++show i)
change_tu (TDebug n) p i = TDebug $ change_tu n p i
change_tu o p i = o

change_tvl tt i = Prelude.map (\t -> change_tv t i) tt
change_tv (TT tt) i = TT $ change_tvl tt i
change_tv (TD n tt) i = TD n $ change_tvl tt i
change_tv (TV n) i = TV (n++show i)
change_tv (TDebug n) i = TDebug $ change_tv n i
change_tv o i = o

ren_tu t = rename_tu t (M.empty, Prelude.map (\x -> x:[]) $ ['a'..'z'])

rename_tu (TD n []) d = (d, TD n [])
rename_tu (TD n (t:ts)) d =
	case rename_tu t d of
		(d2, t) -> case rename_tu (TD n ts) d2 of
								(d3, TD n l) -> (d3, TD n (t:l))
rename_tu (TT []) d = (d, TT [])
rename_tu (TT (t:ts)) d =
	case rename_tu t d of
		(d2, t) -> case rename_tu (TT ts) d2 of
								(d3, TT l) -> (d3, TT (t:l))
rename_tu (TU n) (m, nn) =
	case M.lookup n m of
		Just a -> ((m, nn), TU a)
		Nothing -> ((M.insert n (head nn) m, tail nn), TU $ head nn)
rename_tu o d = (d, o)

check_with_rename o e sv =
	case check o e sv of
		P (_, rm, r) ->
			case ren_tu r of
				(d, r) -> P ("", M.map (\x -> snd $ rename_tu x d) rm, r)
		N a b -> N a b

-- (_*sum (length _) (head _))

res = "res"


