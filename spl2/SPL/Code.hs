--module Code (C (..), St (..), eval0, res) where
module SPL.Code (C (..), St (..), eval, res) where

import Data.Map as M hiding (map, filter)
import SPL.Types

-- eval

eval a@(CNum n _) e = a
eval a@(CStr s _) e = a
eval a@(CBool n _) e = a
eval a@(CList l _) e = a
eval a@(CPair l _) e = a
eval a@(CVal v _) e = 
	case M.lookup v e of
		Just v -> v
		Nothing -> error ("cannot find "++show v)

-- reduce
eval (CL (CL c (K p1) i) (K p2) ii) e = eval (CL c (K (p1++p2)) ii) e

-- apply
eval a@(CL (CInFun i (InFun n f) _) (K p) _) e|i == length p = eval (f (evall p e) e) e
eval a@(CL (CInFun i (InFun b f) ii) (K p) iii) e|i > length p =
	(CL (CInFun i (InFun b f) ii) (K (evall p e)) iii)
eval a@(CL (CInFun i (InFun n f) ii) (K p) iii) e|i < length p =
	eval (CL (eval (f (evall (take i p) e) e) e) (K (drop i p)) iii) e
eval a@(CL (CInfFun (InFun n f) _) (K p) _) e = f (evall p e) e

eval (CL a@(CVal v _) (K p) ii) e = eval (CL (eval a e) (K p) ii) e

-- put
eval a@(CL (CL c (S s) ii) (K p) iii) e|length s > length p = a
eval a@(CL (CL c (S s) ii) (K p) iii) e|length s < length p =
	eval (CL (eval c (putp s (evall (take (length s) p) e) e)) (K (drop (length s) p)) iii) e
eval (CL (CL c (S s) _) (K p) _) e|length s == length p = eval c (putp s (evall p e) e)
eval (CL (CL a R _) (K p) ii) e = eval (CL a (K p) ii) (putp ["_f"] [a] e)
eval (CL (CL a L _) (K [CNum 0 _]) _) e = eval a e
eval (CL a@(CL a2 L _) (K p) ii) e = eval (CL a (K (evall p e)) ii) e

eval a@(CL c (S p) _) e = a
eval a@(CL c L _) e = a
eval a@(CL c R _) e = a
--eval a@(CL c R) e = eval c (putp ["_f"] [a] e)

eval o e = error ("eval: "++show o)

evall l e =
	map (\x -> eval x e) l

putp (v:vs) (c:cs) e = putp vs cs (M.insert v c e)
putp [] [] e = e

res = "test"


