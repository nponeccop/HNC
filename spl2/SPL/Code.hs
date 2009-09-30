--module Code (C (..), St (..), eval0, res) where
module SPL.Code (C (..), St (..), eval, res) where

import qualified Data.Map as M
import SPL.Types

-- eval

eval a@(CF n) e = a
eval a@(CNum n) e = a
eval a@(CStr s) e = a
eval a@(CBool n) e = a
eval a@(CList l) e = a
eval a@(CPair l) e = a
eval a@(CStruct m) e = CStruct $ M.map (\x -> eval x e) m
eval a@(CVal v) e = 
	case M.lookup v e of
		Just v -> v
		Nothing -> error ("cannot find "++show v)

-- reduce
eval (CL (CL c (K p1)) (K p2)) e = eval (CL c (K (p1++p2))) e

-- apply
eval a@(CL (CInFun i (InFun n f)) (K p)) e|i == length p = eval (f (evall p e) e) e
eval a@(CL (CInFun i (InFun b f)) (K p)) e|i > length p =
	(CL (CInFun i (InFun b f)) (K (evall p e)))
eval a@(CL (CInFun i (InFun n f)) (K p)) e|i < length p =
	eval (CL (eval (f (evall (take i p) e) e) e) (K (drop i p))) e
eval a@(CL (CInfFun (InFun n f)) (K p)) e = f (evall p e) e

eval (CL a@(CVal v) (K p)) e = eval (CL (eval a e) (K p)) e
eval (CL a@(CL c (D _)) (K p)) e = eval (CL (eval a e) (K p)) e
eval (CL a@(CDebug _ c) (K p)) e = eval (CL (eval c e) (K p)) e

-- put
eval a@(CL (CL c (S s)) (K p)) e|length s > length p = a
eval a@(CL (CL c (S s)) (K p)) e|length s < length p =
	eval (CL (eval c (putp s (evall (take (length s) p) e) e)) (K (drop (length s) p))) e
eval (CL (CL c (S s)) (K p)) e|length s == length p = eval c (putp s (evall p e) e)
eval (CL (CL a R) (K p)) e = eval (CL a (K p)) (putp ["_f"] [a] e)
eval (CL (CL a L) (K [CNum 0])) e = eval a e
eval (CL a@(CL a2 L) (K p)) e = eval (CL a (K (evall p e))) e

-- struct
eval (CL (CStruct m) (D n)) e =
	case M.lookup n m of
		Just a -> a
		Nothing -> error "CDot $ CStruct"

eval (CL a (D n)) e =
	eval (CL (eval a e) (D n)) e

eval (CL (CStruct m) (W ws)) e|M.null m =
	eval_struct (CStruct m) ws e

-- where
eval (CL c (W [])) e =
	eval c e
eval a@(CL c (W ((n, p):ws))) e =
	eval (CL (CL (CL c (W ws)) (S [n])) (K [p])) e

-- other
eval a@(CL c (S p)) e = a
eval a@(CL c L) e = a
eval a@(CL c R) e = a
--eval a@(CL c R) e = eval c (putp ["_f"] [a] e)

eval (CDebug _ c) e = eval c e

eval o e = error ("eval: "++show o)

eval_struct (CStruct m) [] e =
	CStruct m

eval_struct (CStruct m) ((n,p):ws) e =
	eval_struct (CStruct $ M.insert n (eval p e) m) ws (putp [n] [p] e)

evall l e =
	map (\x -> eval x e) l

putp (v:vs) (c:cs) e = putp vs cs (M.insert v c e)
putp [] [] e = e

res = "test"


