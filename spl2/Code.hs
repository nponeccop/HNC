module Code (C (..), St (..), eval0, res) where

import Data.Map as M hiding (map, filter)
import Types
import BaseFunctions

valBool (CBool b) = b

-- base
do_incr (CNum a:[]) e = CNum (a+1)
do_pair (a:b:[]) e = CPair2 a b
do_mul (CNum a:CNum b:[]) e = CNum (a*b)
do_list l e = CList l
--do_if (a@(CBool True):b@(CL c2 p2):c@(CL c3 p3):[]) e =
--	case eval a e of
--		CBool True -> eval (CL (CVal "force") (K [b])) e
--		CBool False -> do_force [c] e
do_if (a@(CBool a1):b@(CL b1 L):c@(CL c1 L):[]) e =
	case a1 of
		True -> eval b1 e
		False -> eval c1 e
do_if o e = error ("do_if"++show o)
do_force (CL c L:[]) e =
	eval c e
do_less (CNum a:CNum b:[]) e =
	CBool (a<b)
do_is_empty (CList a:[]) e =
	CBool (0 == length a)
do_head (CList a:[]) e =
	head a
do_tail (CList a:[]) e =
	CList (tail a)
do_join (CList a:CList b:[]) e =
	CList (a ++ b)
do_join o e = error ("do_join"++show o)
do_filter (CL a p:CList b:[]) e =
	CList (filter (\x -> valBool (eval (CL (CL a p) (K [x])) e)) b)
do_not (CBool a:[]) e =
	CBool (not a)

base = M.fromList $
	("incr", CL (CInFun 1 (InFun "incr" do_incr)) (K [])):
	("sum", CL (CInFun 2 (InFun "sum" do_sum)) (K [])):
	("pair", CL (CInFun 2 (InFun "pair" do_pair)) (K [])):
	("mul", CL (CInFun 2 (InFun "mul" do_mul)) (K [])):
	("list", CL (CInfFun (InFun "list" do_list)) (K [])):
	("elist", CList []):
	("length", CL (CInFun 1 (InFun "length" do_length)) (K [])):
	("force", CL (CInFun 1 (InFun "force" do_force)) (K [])):
	("if", CL (CInFun 3 (InFun "if" do_if)) (K [])):
	("less", CL (CInFun 2 (InFun "less" do_less)) (K [])):
	("is_empty", CL (CInFun 1 (InFun "is_empty" do_is_empty)) (K [])):
	("head", CL (CInFun 1 (InFun "head" do_head)) (K [])):
	("tail", CL (CInFun 1 (InFun "tail" do_tail)) (K [])):
	("join", CL (CInFun 2 (InFun "join" do_join)) (K [])):
	("joina", CL (CInFun 2 (InFun "joina" do_joina)) (K [])):
	("filter", CL (CInFun 2 (InFun "filter" do_filter)) (K [])):
	("not", CL (CInFun 1 (InFun "not" do_not)) (K [])):
	("to_string", CL (CInFun 1 (InFun "to_string" do_to_string)) (K [])):
	[]

-- eval

eval a@(CNum n) e = a
eval a@(CStr s) e = a
eval a@(CBool n) e = a
eval a@(CList l) e = a
eval a@(CVal v) e = 
	case M.lookup v e of
		Just v -> v
		Nothing -> error ("cannot find "++show v)

-- reduce
eval (CL (CL c (K p1)) (K p2)) e = eval (CL c (K (p1++p2))) e

-- apply
eval a@(CL (CInFun i (InFun n f)) (K p)) e|i == length p = f (evall p e) e
eval a@(CL (CInFun i f) (K p)) e|i > length p = a
eval a@(CL (CInFun i f) (K p)) e|i < length p =
	error ("too many params"++show p)
eval a@(CL (CInfFun (InFun n f)) (K p)) e = f (evall p e) e

eval (CL a@(CVal v) (K p)) e = eval (CL (eval a e) (K p)) e

-- put
eval a@(CL (CL c (S s)) (K p)) e|length s > length p = a
eval a@(CL (CL c (S s)) (K p)) e|length s < length p = error "SK"
eval (CL (CL c (S s)) (K p)) e|length s == length p = eval c (putp s (evall p e) e)
eval (CL (CL a@(CL c (S s)) M) (K p)) e|length s == length p = eval c (putp ["_f"] [a] (putp s (evall p e) e))-- I think it is not correct

eval a@(CL c (S p)) e = a
eval a@(CL c L) e = a
eval a@(CL c M) e = a

eval o e = error ("eval: "++show o)

evall l e =
	map (\x -> eval x e) l

putp (v:vs) (c:cs) e = putp vs cs (M.insert v c e)
putp [] [] e = e

eval0 c =
	eval c BaseFunctions.get_codes

ts = [
	CL (CVal "sum") (K [CNum 2])
	,CL (CVal "sum") (K [CNum 2, CNum 3])
	,CL (CL (CL (CVal "sum") (K [CNum 2, CVal "x"])) (S ["x"])) (K [CNum 1])
	,CL (CVal "force") (K [CL (CL (CVal "sum") (K [CNum 1, CNum 2])) L])
	,CL (CVal "filter") (K [CL (CVal "less") (K [CNum 9]), CList [CNum 9, CNum 10, CNum 11]])
	,CL (CL (CL (CVal "_f") (K [CNum 1])) (S ["_f"])) (K [
		CL (CL (CVal "if") (K [CL (CVal "less") (K [CNum 1, CVal "_"])
			,CL (CNum 1) L
			,CL (CNum 2) L
			])) (S ["_"])
		])
	,CL (CL (CL (CVal "_z") (K [CList [CNum 8, CNum 9, CNum 4, CNum 4, CNum 5, CNum 3]])) (S ["_z"])) (K [
		CL (CL (CL (CVal "if") (K [CL (CVal "is_empty") (K [CVal "_"])
			,CL (CList []) L
			,CL (CL (CL (
				CL (CVal "join") (K [
					CL (CVal "_f") (K [CL (CVal "filter") (K [CL (CL (CVal "not") (K [CL (CVal "less") (K [CVal "h", CVal "_"])])) (S ["_"]), CVal "t"])])
					,CL (CVal "join") (K [
						CL (CVal "list") (K [CVal "h"])
						,CL (CVal "_f") (K [CL (CVal "filter") (K [CL (CVal "less") (K [CVal "h"]), CVal "t"])])
						])
					])
				) (S ["h", "t"])) (
					K [
						CL (CVal "head") (K [CVal "_"])
						,CL (CVal "tail") (K [CVal "_"])
						])) L
			])) (S ["_"])) M
		])
	]

{-
 - if (.less 0.length) (.sum 2) (,sum 2 _) | _:1
 - -}

res = show $ eval (ts!!6) BaseFunctions.get_codes


