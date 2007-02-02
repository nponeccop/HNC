module Parser where

import Data.Map as M

data STreeLambdaMod = StN | StL | StSN | StSL
	deriving Show

data STree = Sn [Char] Int | Snum Int Int | Sbool Bool Int
	| Sfun Bool STree [STree] [STree] Int
	| Slambda STreeLambdaMod STree [STree] [STree] Int
	| Sset [Char] STree Int
	| Sl [STree] Int
	deriving Show

tv (Sn n i) = n
tvl (Sl l i) = l

-- parser
p_and :: [Tokens] -> [Char] -> Int -> Maybe (Int, [STree])
p_and [] s o = Just (0,[])
p_and (t:ts) s o =
	case call t s o of
		Just (l,v) ->
			(case p_and ts s (o+l) of
				Just (ls,vs) -> Just ((l+ls),(v:vs))  -- Just ls -> Just ((l,v):ls)
				Nothing -> Nothing)
		Nothing -> Nothing

p_or :: [([Tokens], Int -> [STree] -> (Int, STree))] -> [Char] -> Int -> Maybe (Int, STree)
p_or [] _ _ = Nothing
p_or ((ts,f):ls) s o =
  case p_and ts s o of
    Just (ls,vs) -> Just (f ls vs)
    Nothing -> p_or ls s o

parse s =
  p_or [([Texpr],(\ls vs -> (ls, vs!!0)))]
       s 0

{- end of parse -}

data Tokens = Ts1|Ts|Tsn|Tc1|Tc|Td1|Tdpos|Tdmin|Td
	|Tcc Char|Tss [Char]
	|Texpr|Tparams|Texprpar|Texprblock|Twhere|Tset
	deriving Show

-- basic tokens
call::Tokens -> [Char] -> Int -> Maybe (Int, STree)
call (Tcc c) s o|o < length s && c == s!!o = Just (1, Sn [s!!o] o)
call Ts1 s o|o < length s && s!!o `elem` " " = Just (1, Sn [s!!o] o)
call Tc1 s o|o < length s && s!!o `elem` "_abcdefghijklmnopqrstuvwxyz" = Just (1, Sn [s!!o] o)
call Td1 s o| o < length s && s!!o `elem` "1234567890" = Just (1, Sn [s!!o] o)
call Tc s o =
	p_or [([Tc1,Tc],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)) o))),
		([Tc1],(\ls vs -> (ls, vs!!0)))]
		s o
call Ts s o =
	p_or [([Ts1,Ts],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)) o))),
		([Ts1],(\ls vs -> (ls, vs!!0)))]
		s o
call Tsn s o =
	p_or [([Ts1,Tsn],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)) o))),
		([],(\ls vs -> (ls, Sn "" o)))]
		s o
call (Tss "") s o = Just (0, Sn "" o)
call (Tss (x:xs)) s o =
	p_or [([Tcc x,Tss xs], (\ls vs -> (ls, Sn ((tv (vs!!0))++(tv (vs!!1))) o)))]
		s o
call Tdpos s o =
	p_or [([Td1,Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)) o))),
		([Td1],(\ls vs -> (ls, vs!!0)))] -- was [] here
		s o
call Tdmin s o =
	p_or [([Tcc '-',Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)) o)))]
		s o
call Td s o =
	p_or [([Tdmin],(\ls vs -> (ls, Snum (read (tv (vs!!0))) o))),
		([Tdpos],(\ls vs -> (ls, Snum (read (tv (vs!!0))) o)))]
		s o

-- main tokens
call Texpr s o =
	p_or [
		([Tss ".",Texpr,Tparams,Twhere], \ls vs -> (ls, Sfun False (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss ",",Texpr,Tparams,Twhere], \ls vs -> (ls, Slambda StN (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss "^",Texpr,Tparams,Twhere], \ls vs -> (ls, Slambda StSN (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss "~",Texpr,Tparams,Twhere], \ls vs -> (ls, Slambda StL (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss "(",Texpr,Tss ")"], \ls vs -> (ls, vs!!1)),
		([Tc], \ls vs -> (ls, vs!!0)),
		([Td], \ls vs -> (ls, vs!!0))]
		s o

call Texprpar s o =
	p_or [
		([Tss ".",Texprpar,Tparams], \ls vs -> (ls, Sfun False (vs!!1) (tvl (vs!!2)) [] o)),
		([Tss ",",Texprpar,Tparams], \ls vs -> (ls, Slambda StN (vs!!1) (tvl (vs!!2)) [] o)),
		([Tss "^",Texprpar,Tparams], \ls vs -> (ls, Slambda StSN (vs!!1) (tvl (vs!!2)) [] o)),
		([Tss "~",Texprpar,Tparams], \ls vs -> (ls, Slambda StL (vs!!1) (tvl (vs!!2)) [] o)),
		([Tss "(",Texprblock,Tss ")"], \ls vs -> (ls, vs!!1)),
		([Tc], \ls vs -> (ls, vs!!0)),
		([Td], \ls vs -> (ls, vs!!0))]
		s o

call Texprblock s o =
	p_or [
		([Tss ".",Texpr,Tparams,Twhere], \ls vs -> (ls, Sfun True (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss ",",Texpr,Tparams,Twhere], \ls vs -> (ls, Slambda StN (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss "^",Texpr,Tparams,Twhere], \ls vs -> (ls, Slambda StSN (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss "~",Texpr,Tparams,Twhere], \ls vs -> (ls, Slambda StL (vs!!1) (tvl (vs!!2)) (tvl (vs!!3)) o)),
		([Tss "(",Texpr,Tss ")"], \ls vs -> (ls, vs!!1)),
		([Tc], \ls vs -> (ls, vs!!0)),
		([Td], \ls vs -> (ls, vs!!0))]
		s o

call Tparams s o =
	p_or [
		([Tsn,Texprpar,Tparams], \ls vs -> (ls, Sl ((vs!!1):(tvl (vs!!2))) o)),
		([Tsn,Texprpar], \ls vs -> (ls, Sl ((vs!!1):[]) o)),
		([], \ls vs -> (ls, Sl [] o))]
		s o

call Twhere s o =
	p_or [
		([Tsn,Tss "|",Tset,Twhere], \ls vs -> (ls, Sl ((vs!!2):(tvl (vs!!3))) o)),
		([Tsn,Tss "|",Tset], \ls vs -> (ls, Sl ((vs!!2):[]) o)),
		([], \ls vs -> (ls, Sl [] o))]
		s o
call Tset s o =
	p_or [
		([Tc,Tss ":",Texprpar], \ls vs -> (ls, Sset (tv (vs!!0)) (vs!!2) o)),
		([], \ls vs -> (ls, Sl [] o))]
		s o

call _ _ _ = Nothing

