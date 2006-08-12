
module E1 (parse) where

{- parse -}

data Syntax = Sn [Char] | Snum Int | Sfun Syntax [Syntax] | Sl [Syntax] deriving Show
data Tokens = Ts1|Topen|Tclose|Ts|Tc1|Tc|Tdot|Tmin|Td1|Td2|Tdpos|Tdmin|Td
	|Tval|Top|Topnd|Texpr|Tvals
	deriving Show

tv (Sn s) = s
tv (Snum d) = show d
tv (Sfun s p) = show s
tvl (Sl s) = s

call::Tokens -> [Char] -> Int -> Maybe (Int, Syntax)
call Topen s o|o < length s && '(' == s!!o = Just (1, Sn [s!!o])
call Tclose s o|o < length s && ')' == s!!o = Just (1, Sn [s!!o])
call Tdot s o|o < length s && '.' == s!!o = Just (1, Sn [s!!o])
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
call Tval s o =
	p_or [([Texpr],(\ls vs -> (ls, vs!!0))),
		([Tc],(\ls vs -> (ls, vs!!0))),
		([Td],(\ls vs -> (ls, vs!!0)))]
		s o
call Tvals s o =
	p_or [([Tval,Ts,Tvals],(\ls vs -> (ls, Sl (vs!!0:tvl (vs!!2))))),
		([Tval],(\ls vs -> (ls, Sl [vs!!0])))]
		s o
call Texpr s o =
	p_or [([Topen,Tval,Ts,Tvals,Tclose],(\ls vs-> (ls, Sfun ((vs!!1)) (tvl (vs!!3))))),
		([Topen,Tval,Tclose],(\ls vs-> (ls, Sfun (vs!!1) [])))]
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

data Eval = Enum Int | En [Char] | Elambda Eval [Eval] | Epair Eval Eval deriving Show

run_fun (En "add_fun") ((Enum p1):(Enum p2):[]) = Enum (p1 + p2)
run_fun (En "pair_fun") (p1:p2:[]) = Epair p1 p2
run_fun (En "cond_fun") ps =
	map (fun (pc,pe) ->
		case pc of
		0 -> 
	) ps
run_fun e (p:[]) = Elambda e (p:[])
run_fun e [] = Elambda e []

check_lambda (Elambda f l) = run_fun f l
check_lambda o = o

eval::Syntax -> Eval
eval (Sn "add") = Elambda (En "add_fun") []
eval (Sn "pair") = Elambda (En "pair_fun") []
eval (Sn "cond") = Elambda (En "cond_fun") []
eval (Sn o) = En ("unknown: " ++ o)

eval (Snum n) = Enum n
eval (Sfun s []) = check_lambda (eval s)
eval (Sfun s pp@(p:ps)) =
	case eval s of
		Elambda f l -> check_lambda (Elambda f ((map eval pp)++l))
		_ -> En "unknown2"
{-		o -> check_lambda (case eval (Sfun p ps) of
												Elambda f l -> Elambda f (o:l)
												_ -> En "unknown2")-}

run s =
	case parse s of
		Just (i,v) -> Just (eval v)
		Nothing -> Nothing

str = "(add 1 2)"
s = parse str
e = run str

{- end of eval -}


