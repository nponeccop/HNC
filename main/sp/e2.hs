
{- syntax
call function:

,filter [,more 14] ,map square ,list 1 2 3 4 5


-}

module E1 (parse) where

{- parse -}

data Syntax = Sn [Char] | Snum Int | Sfun Syntax [Syntax] | Slambda Syntax [Syntax]
	| Sl [Syntax] deriving Show
data Tokens = Ts1|Topen|Tclose|Topen2|Tclose2|Topen3|Tclose3
	|Ts|Tsn|Tc1|Tc|Tdot|Tcomma|Tdotcom|Tmin|Td1|Td2|Tdpos|Tdmin|Td
	|Tval|Texpr|Tparams
	deriving Show

tv (Sn s) = s
tv (Snum d) = show d
tv (Sfun s p) = show s
tvl (Sl s) = s

call::Tokens -> [Char] -> Int -> Maybe (Int, Syntax)
call Topen s o|o < length s && '(' == s!!o = Just (1, Sn [s!!o])
call Tclose s o|o < length s && ')' == s!!o = Just (1, Sn [s!!o])
call Topen2 s o|o < length s && '[' == s!!o = Just (1, Sn [s!!o])
call Tclose2 s o|o < length s && ']' == s!!o = Just (1, Sn [s!!o])
call Topen3 s o|o < length s && '{' == s!!o = Just (1, Sn [s!!o])
call Tclose3 s o|o < length s && '}' == s!!o = Just (1, Sn [s!!o])
call Tdot s o|o < length s && '.' == s!!o = Just (1, Sn [s!!o])
call Tcomma s o|o < length s && ',' == s!!o = Just (1, Sn [s!!o])
call Tdotcom s o|o < length s && ';' == s!!o = Just (1, Sn [s!!o])
call Tmin s o|o < length s && '-' == s!!o = Just (1, Sn [s!!o])
call Ts1 s o|o < length s && s!!o `elem` " \t" = Just (1, Sn [s!!o])
call Tc1 s o|o < length s && s!!o `elem` "abcdefghijklmnopqrstuvwxyz" = Just (1, Sn [s!!o])
call Td1 s o| o < length s && s!!o `elem` "1234567890" = Just (1, Sn [s!!o])
call Tc s o =
	p_or [([Tc1,Tc],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Tc1],(\ls vs -> (ls, vs!!0)))]
		s o
call Ts s o =
	p_or [([Ts1,Ts],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Ts1],(\ls vs -> (ls, vs!!0)))]
		s o
call Tsn s o =
	p_or [([Ts1,Tsn],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([],(\ls vs -> (ls, Sn "")))]
		s o
call Td2 s o =
	p_or [([Td1,Td1],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)))))] s o
call Tdpos s o =
	p_or [([Td1,Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Td1],(\ls vs -> (ls, vs!!0)))] -- was [] here
		s o
call Tdmin s o =
	p_or [([Tmin,Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)))))]
		s o
call Td s o =
	p_or [([Tdmin],(\ls vs -> (ls, Snum (read (tv (vs!!0)))))),
		([Tdpos],(\ls vs -> (ls, Snum (read (tv (vs!!0))))))]
		s o

{-S-expr call Texpr s o =
	p_or [([Topen,Texpr,Ts,Tparams,Tclose], \ls vs -> (ls, Sfun (vs!!1) (tvl (vs!!3)))),
				([Tc], \ls vs -> (ls, vs!!0))]
		s o

call Tparams s o =
	p_or [([Texpr,Ts,Tparams], \ls vs -> (ls, Sl ((vs!!0):(tvl (vs!!2))))),
				([Texpr], \ls vs -> (ls, Sl ((vs!!0):[]))),
				([], \ls vs -> (ls, Sl []))]
		s o-}

call Texpr s o =
	p_or [([Tcomma,Texpr,Tparams], \ls vs -> (ls, Sfun (vs!!1) (tvl (vs!!2)))),
				([Topen2,Texpr,Tclose2], \ls vs -> (ls, Slambda (vs!!1) [])),
				([Tc], \ls vs -> (ls, vs!!0)),
				([Tc,Tdpos], \ls vs -> (ls, vs!!0)),
				([Td], \ls vs -> (ls, vs!!0))]
		s o

call Tparams s o =
	p_or [([Tsn,Texpr,Tparams], \ls vs -> (ls, Sl ((vs!!1):(tvl (vs!!2))))),
				([Tsn,Texpr], \ls vs -> (ls, Sl ((vs!!1):[]))),
				([], \ls vs -> (ls, Sl []))]
		s o
			

call _ _ _ = Nothing

p_and :: [Tokens] -> [Char] -> Int -> Maybe (Int, [Syntax])
p_and [] s o = Just (0,[])
p_and (t:ts) s o =
	case call t s o of
		Just (l,v) ->
			(case p_and ts s (o+l) of
				Just (ls,vs) -> Just ((l+ls),(v:vs))  -- Just ls -> Just ((l,v):ls)
				Nothing -> Nothing)
		Nothing -> Nothing

p_or :: [([Tokens], Int -> [Syntax] -> (Int, Syntax))] -> [Char] -> Int -> Maybe (Int, Syntax)
p_or [] _ _ = Nothing
p_or ((ts,f):ls) s o =
  case p_and ts s o of
    Just (ls,vs) -> Just (f ls vs)
    Nothing -> p_or ls s o

parse s =
  p_or [([Texpr],(\ls vs -> (ls, vs!!0)))]
       s 0

{- end of parse -}

{- eval -}

data Eval = Enum Int | En [Char] | Eb Bool
	| Elambda Eval [Eval] | Elist [Eval]
	deriving Show

run_fun::Eval -> [Eval] -> Eval
run_fun (En "sum_fun") (Enum p1:Enum p2:[]) = Enum ((+) p1 p2)
run_fun (En "square_fun") (Enum p:[]) = Enum ((*) p p)
run_fun (En "list_fun") ps = Elist ps
run_fun (En "not_fun") (Eb p:[]) = Eb (not p)
run_fun (En "more_fun") (Enum p1:Enum p2:[]) = Eb ((>) p1 p2)
run_fun (En "filter_fun") (Elambda f l:Elist ps:[]) = Elist (filter (\x ->
	case check_lambda (Elambda f (l++[x])) of 
	Eb True -> True
	Eb False -> False) ps)
run_fun (En "map_fun") (f:Elist l:[]) = Elist (map (\e -> check_lambda (Elambda f [e])) l)
run_fun e (p:[]) = Elambda e (p:[])
run_fun e [] = Elambda e []

check_lambda (Elambda (Elambda f l1) l2) = check_lambda (Elambda f (l1++l2))
check_lambda (Elambda f l) = run_fun f l
check_lambda o = o

eval::Syntax -> Eval
eval (Sn fn) = Elambda (En (fn++"_fun")) []
eval (Snum n) = Enum n
eval (Slambda s []) = check_lambda (eval s)
eval (Slambda s []) = check_lambda (eval s)
eval (Slambda s pp@(p:ps)) =
	case eval s of
		Elambda f l -> check_lambda (Elambda f ((map eval pp)++l))
		_ -> En "trying to call non-function"

eval (Sfun s []) = check_lambda (eval s)
eval (Sfun s pp@(p:ps)) =
	case eval s of
		Elambda f l -> check_lambda (Elambda f ((map eval pp)++l))
		_ -> En "trying to call non-function"


run s =
	case parse s of
		Just (i,v) -> Just (eval v)
		Nothing -> Nothing


-- str = ",filter [,not,more 14],map square ,list 1 2 3 4 5"
str = ",[,not,more 14] 15"
s = parse str
e = run str

{- end of eval -}

f1 = not
f2 a b = (>) a b
f3 = (+)

fun1 x = f1 (f2 15 (f3 10 x))
u fa fb z = fa (fb z)
fun2 = (u f1 (u (f2 15) (f3 10)))
fun3 = (u f1 (u (f2 15) (f3 10)))

{-
fun x = (f1 p1a p1b... (f2 p2a... (f3 p3a... (fn pna... x))))
(.) (f1 p1a p1b...) ((.) (f2 p2a...) (fn pna...))
-}




