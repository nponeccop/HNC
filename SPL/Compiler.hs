{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SPL.Compiler (P (..), compile, remove_cdebug, remove_ctyped, res) where

import SPL.Types
import SPL.Parser2 hiding (P (..), res)
import SPL.Code hiding (res)
import qualified Data.Map as M
import SPL.Lib.Bignum (lib)
import SPL.Lib.Str (lib)

data P = P C | N Int [Char]
	deriving Show

comp (Sn x i) u e =
	P $ CDebug i $ CNum x
comp (Sb x i) u e =
	P $ CDebug i $ CBool x
comp (Sstr s i) u e =
	P $ CDebug i $ CStr s
comp (Scall (Ss "ffi" _) (SynK [(Sstr t _), Sstr ffn _]) _) u e =
	case M.lookup ffn lib of
		Just f ->
			let t2 = read t::SPL.Types.T in
				case t2 of
					TT l -> P $ CL (CInFun ((-) (length l) 1) (InFun ("ffi_"++t) f)) (K [])
					o -> error "ffi_apply: type"
		Nothing -> error ("ffi: "++ffn++" did not find in lib")
	where
		lib = M.fromList $ SPL.Lib.Bignum.lib ++ SPL.Lib.Str.lib

comp (Sdot (Sroot _) p i) u e =
	comp (Ss p i) [] e

comp (Ss s i) u e =
	case f u of
		Just v -> P $ CDebug i v
		Nothing ->
			case M.lookup s e of
				Just e -> P $ CDebug i $ CVal s
				Nothing -> N i $ "unknown function: "++s
	where
		f [] = Nothing
		f ((m:ms):us) =
			case M.lookup m e of
				Just (TS w) ->
					case f2 (CVal m) ms w of
						Just r -> Just r
						Nothing -> f us
				Nothing -> Nothing
		f2 m1 [] e =
			case M.lookup s e of
				Just _ -> Just $ CL m1 (D s)
				Nothing -> Nothing
		f2 m1 (m:ms) e =
			case M.lookup m (e::M.Map [Char] T) of
				Just (TS e2) ->
					case f2 (CL m1 (D m)) ms e2 of
						Just v -> Just v
						Nothing -> Nothing
				Nothing -> Nothing

-- Sdot (Sval "bn") (D "int")

comp (Sstruct l t i) u e =
--	CDebug i $ CStruct $ M.fromList $ map (\(Sset k v _) -> (k, comp v)) l
	let t2 =
		case comp (Stype (Sn 1 0) t 0) u e of
			P c ->
				case remove_cdebug c of
					CL _ (W l) -> l
					o -> error "3"
			N i e -> error "2"
	in
	case fn l e of
		Right w -> P $ CDebug i $ CL (CStruct M.empty) $ W (w++t2)
		Left e -> e
	where
		fn [] e = Right []
		fn ((Sset k v i):l) e =
			case comp v u e of
				P c ->
					case fn l $ putp [k] e of
						Right v -> Right $ (k, c):v
						o -> o
				N i e -> Left $ N i e
comp (Sdot v p i) u e =
	case comp v u e of
		P r -> P $ CDebug i $ CL r (D p)
		N i e -> N i e
comp (Scall f (SynK a) i) u e =
--	CDebug i $ CL (comp f u e) (K (map (\x -> comp x u e) a))
	case comp f u e of
		P r ->
			case fn a of
				Right l -> P $ CDebug i $ CL r (K l)
				Left (N i e) -> N i e
		N i e -> N i e
	where
		fn [] = Right []
		fn (x:xs) =
			case comp x u e of
				P x ->
					case fn xs of
						Right l -> Right (x:l)
						o -> o
				N i e -> Left $ N i e
comp (Scall f (SynS a) i) u e =
	case comp f u (putp a e) of
		P r -> P $ CDebug i $ CL r (S a)
		N i e -> N i e
comp (Scall f (SynW a) i) u e =
--	CDebug i $ CL (comp f u e) (W $ map (\(Sset k v _) -> (k, comp v u e)) a)
	case fn a e of
		Right l ->
			case comp f u $ putp (map fst l) e of
				P r -> P $ CDebug i $ CL r (W l)
				N i e -> N i e
		Left (N i e) -> N i e
	where
		fn [] e = Right []
		fn ((Sset k v _):l) e =
			case comp v u e of
				P c ->
					case fn l $ putp [k] e of
						Right v -> Right $ (k, c):v
						o -> o
				N i e -> Left $ N i e
comp (Stype f a i) u e =
	case fn a e of
		Right l ->
			case comp f u $ putp (map fst l) e of
				P r -> P $ CDebug i $ CL r (W l)
				N i e -> N i e
		Left (N i e) -> N i e
	where
		type_name = foldr (++) "" $ map (\(Sset k (Sn n _) _) -> k++show n) a
		fn [] e = Right []
		fn ((Sset k (Sn n _) _):l) e =
					case fn l $ putp [k] e of
						Right v -> Right $ (("mk_"++k), mktp k n):(("if_"++k), mkif k n):v
						o -> o
		mktp k n =
			CL (CInFun (n+1) (InFun ("typ_"++ft) do_type)) (K[CStr k])
			where
				ft = "TT [T \"string\", "
					++ (foldr (\a b -> a++", "++b) "" $ map (\x -> "TU \""++type_name++"_"++show x++"\"") $ take n ([1..]::[Int]))
					++"T \""++type_name++"\"]"
				do_type l e =
					CList l
		mkif k n =
			CL (CInFun 4 (InFun ("ift_"++ft) do_ift)) (K[CStr k])
			where
--				ft = "TT [T \"string\", " -- ++"TT ["
--					++"T \""++type_name++"\""
--					++", TT [TL, TU \"zzz\"], TT [TL, TU \"zzz\"], TU \"zzz\"]"
				ft = "TT [T \"string\", "++"TT ["
					++ (foldr (\a b -> a++", "++b) "" $ map (\x -> "TU \""++type_name++"_"++show x++"\"") $ take n [(1::Int)..])
--					++"T \""++type_name++"\", "
					++"TU \"zzz\"], TT [TL, TU \"zzz\"], T \""++type_name++"\", TU \"zzz\"]"
				do_ift ((CStr t):f:f2:(CList ((CStr t2):ts)):[]) e =
					case t == t2 of
						True -> eval (CL f (K ts)) e
						False -> eval (CL f2 (K [CVal "go"])) e
				do_ift o e = error $ "E: "++show o


comp (Scall f (SynM a) i) u e =
	case comp f u $ putp ["_f"] e of
		P r -> P $ CDebug i $ CL r R
		N i e -> N i e
comp (Scall f SynL i) u e =
	case comp f u e of
		P r -> P $ CDebug i $ CL r L
		N i e -> N i e
comp (Scall f (SynU u) i) u2 e =
	case comp f (u++u2) e of
		P r -> P $ CDebug i r
		N i e -> N i e

compile c = comp c []

putp [] e =
	e
putp (p:ps) e =
	putp ps $ M.insert p (T "") e

r_d (CDebug _ c) = r_d c
r_d (CStruct m) = CStruct $ M.map r_d m
r_d (CL c (K p)) =
	CL (r_d c) (K (map r_d p))
r_d (CL c (S a)) =
	CL (r_d c) (S a)
r_d (CL c (W a)) =
	CL (r_d c) (W (map (\(a,b) -> (a, r_d b)) a))
r_d (CL c (D n)) =
	CL (r_d c) (D n)
r_d (CL c R) =
	CL (r_d c) R
r_d (CL c L) =
	CL (r_d c) L
r_d o = o

remove_cdebug = r_d

r_t (CDebug _ c) = r_t c
r_t (CStruct m) = CStruct $ M.map r_t m
r_t (CL c (K p)) =
	CL (r_t c) (K (map r_t p))
r_t (CL c (S a)) =
	CL (r_t c) (S a)
r_t (CL c (W a)) =
	CL (r_t c) (W (map (\(a,b) -> (a, r_t b)) a))
r_t (CL c (D n)) =
	CL (r_t c) (D n)
r_t (CL c R) =
	CL (r_t c) R
r_t (CL c L) =
	CL (r_t c) L
r_t o = o
remove_ctyped = r_t

res = "res"


