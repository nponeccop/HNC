module Parser where

import Structure
import Data.Map as M

-- parser
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

data Tokens = Ts1|Ts|Tsn|Tc1|Tc|Td1|Tdpos|Tdmin|Td
	|Tcc Char|Tss [Char]
	|Texpr|Tparams
	deriving Show

-- basic tokens
call::Tokens -> [Char] -> Int -> Maybe (Int, Syntax)
call (Tcc c) s o|o < length s && c == s!!o = Just (1, Sn [s!!o])
call Ts1 s o|o < length s && s!!o `elem` " " = Just (1, Sn [s!!o])
call Tc1 s o|o < length s && s!!o `elem` "_abcdefghijklmnopqrstuvwxyz" = Just (1, Sn [s!!o])
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
call (Tss "") s o = Just (0, Sn "")
call (Tss (x:xs)) s o =
	p_or [([Tcc x,Tss xs], (\ls vs -> (ls, Sn ((tv (vs!!0))++(tv (vs!!1))))))]
		s o
call Tdpos s o =
	p_or [([Td1,Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1))))),
		([Td1],(\ls vs -> (ls, vs!!0)))] -- was [] here
		s o
call Tdmin s o =
	p_or [([Tcc '-',Tdpos],(\ls vs -> (ls, Sn (tv (vs!!0)++tv (vs!!1)))))]
		s o
call Td s o =
	p_or [([Tdmin],(\ls vs -> (ls, Snum (read (tv (vs!!0)))))),
		([Tdpos],(\ls vs -> (ls, Snum (read (tv (vs!!0))))))]
		s o

-- main tokens
call Texpr s o =
	p_or [
		([Tcc '.',Texpr,Tparams], \ls vs -> (ls, Sfun (vs!!1) (tvl (vs!!2)))),
		([Tcc ',',Texpr,Tparams], \ls vs -> (ls, Slambda N (vs!!1) (tvl (vs!!2)))),
		([Tcc '~',Texpr,Tparams], \ls vs -> (ls, Slambda L (vs!!1) (tvl (vs!!2)))),
		([Tcc '(',Texpr,Tcc ')'], \ls vs -> (ls, vs!!1)),
		([Tc], \ls vs -> (ls, vs!!0)),
		([Td], \ls vs -> (ls, vs!!0))]
		s o

call Tparams s o =
	p_or [
		([Tsn,Texpr,Tparams], \ls vs -> (ls, Sl ((vs!!1):(tvl (vs!!2))))),
		([Tsn,Texpr], \ls vs -> (ls, Sl ((vs!!1):[]))),
		([], \ls vs -> (ls, Sl []))]
		s o

call _ _ _ = Nothing

