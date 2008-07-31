module Code (C (..), St (..), eval0, res) where

import Data.Map as M hiding (map, filter)
import Types
import BaseFunctions

-- eval

eval a@(CNum n) e = a
eval a@(CStr s) e = a
eval a@(CBool n) e = a
eval a@(CList l) e = a
eval a@(CPair l) e = a
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

-- put
eval a@(CL (CL c (S s)) (K p)) e|length s > length p = a
eval a@(CL (CL c (S s)) (K p)) e|length s < length p =
	eval (CL (eval c (putp s (evall (take (length s) p) e) e)) (K (drop (length s) p))) e
eval (CL (CL c (S s)) (K p)) e|length s == length p = eval c (putp s (evall p e) e)
eval (CL (CL a R) (K p)) e = eval (CL a (K p)) (putp ["_f"] [a] e)
eval (CL (CL a L) (K [CNum 0])) e = eval a e
eval (CL a@(CL a2 L) (K p)) e = eval (CL a (K (evall p e))) e

eval a@(CL c (S p)) e = a
eval a@(CL c L) e = a
eval a@(CL c R) e = eval c (putp ["_f"] [c] e)

eval o e = error ("eval: "++show o)

evall l e =
	map (\x -> eval x e) l

putp (v:vs) (c:cs) e = putp vs cs (M.insert v c e)
putp [] [] e = e

eval0 c =
	eval c BaseFunctions.get_codes

res = "test"


