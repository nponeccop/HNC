module Parser (P (..), Syntax (..), SynParams (..), SynMark(..), parse, res) where

import Hugs.Observe

data SynMark =
	MarkR
	deriving (Eq, Show)

data SynParams =
	SynK [Syntax]
	| SynS [[Char]]
	| SynL
	| SynM [SynMark]
	deriving (Eq, Show)

data Syntax =
	Sc Char
	| Sb Bool
	| Ss [Char]
	| Sstr [Char]
	| Sn Int
	| Sl [Syntax]
	| Sval [Char]
	| Scall Syntax SynParams
	deriving (Eq, Show)

data P = P Int Syntax | N Int
	deriving (Eq, Show)

data Token =
	Ts [Char]
	| Tc Char
	| Tb
	| Tc1
	| Tsp
	| Tspn
	| Tn
	| Tnpos
	| Tnneg
	| Tnum
	| Tstring
	| Tcs
	| Tcon
	| Tval
	| Tpair
	| Tcall
	| Texpr
	| Texpr_top
	| Tparams
	| Tsave
	| Tsave_args
	| Tmark
	| Tmarks
	| Eos
	deriving Show

p_and ((t:ts),f) vi vs s i =
	case call t s i of
		P i1 s1 -> p_and (ts,f) (i1+vi) (s1:vs) s (i+i1)
		N i2 -> N i2
p_and ([],f) vi vs s i =
	P vi (f (reverse vs))
	
p_or (o:os) s i =
	case p_and o 0 [] s i of
		P i s -> P i s
		N i2 -> p_or os s i
p_or [] s i =
	N i

call (Tc c) s i| i < length s && c == s!!i = P 1 (Sc c)
call (Tc c) s i = N i
call Tc1 s i =
	p_or (map (\x -> ([Tc x], \vs -> vs!!0)) "_abcdefghijklmnopqrstuvwxyz") s i
call Tb s i =
	p_or [
		([Tc '1', Tc 'b'], \(c1:c2:[]) -> Sb True)
		,([Tc '0', Tc 'b'], \(c1:c2:[]) -> Sb False)
		] s i
call Tsp s i =
	p_or [
		([Tc ' ', Tsp], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc ' '], \(Sc c:[]) -> Ss (c:""))
		,([Tc '\n'], \(Sc c:[]) -> Ss (c:""))
		] s i
call Tspn s i =
	p_or [
		([Tsp], \(Ss s:[]) -> Ss s)
		,([], \([]) -> Ss "")
		] s i
call Tcs s i =
	p_or [
		([Tc1, Tcs], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc1], \(Sc c:[]) -> Ss (c:""))
		] s i
call Tn s i =
	p_or (map (\x -> ([Tc x], \(Sc c:[]) -> Sn (read (c:"")))) "0123456789") s i
call Tnpos s i =
	p_or [
		([Tn, Tnpos], \(Sn n:Sn n2:[]) -> Sn (n2 + 10 * n))
		,([Tn], \(sn:[]) -> sn)
		] s i
call Tnneg s i =
	p_or [
		([Tc '-', Tnpos], \(_:Sn n:[]) -> Sn (-n))
		] s i
call Tnum s i =
	p_or [
		([Tnpos], \(sn:[]) -> sn)
		,([Tnneg], \(sn:[]) -> sn)
		] s i
call Tstring s i =
    p_or [
        ([Tc '\'', Tcs, Tc '\''], \(Sc c1:Ss sn:Sc c2:[]) -> Sstr sn)
		] s i
call Eos s i| i == length s = P 0 (Ss "")
call Eos s i = N i


call Tparams s i =
	p_or [
		([Tsp,Tval,Tparams], \(_:v:Sl l:[]) -> Sl (v:l))
--		,([Tsp,Tval], \(_:v:[]) -> Sl (v:[]))
		,([Tc ',',Texpr], \(_:c:[]) -> Sl (c:[]))
		,([], \([]) -> Sl [])
		] s i
call Tval s i =
	p_or [
		([Tb], \(b:[]) -> b)
		,([Tnum], \(n:[]) -> n)
		,([Tstring], \(n:[]) -> n)
		,([Tcs,Tnpos], \(Ss s:Sn n:[]) -> Ss $ s++show n) -- create new token?
		,([Tcs], \(s:[]) -> s)
		,([Tc '(', Texpr_top, Tc ')'], \(_:e:_:[]) -> e)
		] s i
call Texpr s i =
	p_or [
--		([Tcall,Tsave_args], \(c:Sl w:[]) -> Scall c (SynS (map (\(Ss s) -> s) w)))
		([Tval,Tparams], \(v:Sl a:[]) ->
			case a of
				[] -> v
				_ -> Scall v (SynK a))
		] s i
call Tsave_args s i =
	p_or [
		([Tcs,Tc '*',Tsave_args], \(c:_:Sl l:[]) -> Sl (c:l))
		,([], \([]) -> Sl [])
--		,([Tc '*',Tcs], \(_:c:[]) -> Sl [c])
		] s i
call Tmarks s i =
	p_or [
		([Tc 'r',Tc '!',Tmarks], \(c:_:Sl l:[]) -> Sl (Sc 'r':l))
		,([Tc 'l',Tc '!',Tmarks], \(c:_:Sl l:[]) -> Sl (Sc 'l':l))
		,([], \([]) -> Sl [])
		] s i
call Tsave s i =
	p_or [
		([Tsave_args,Texpr], \(Sl w:e:[]) ->
			case w of
				[] -> e
				x:xs -> Scall e (SynS (map (\(Ss s) -> s) w)))
		] s i
call Tmark s i =
	p_or [
		([Tmarks,Tsave], \(Sl m:e:[]) ->
			case m of
				(Sc 'r':[]) -> Scall e (SynM [MarkR])
				(Sc 'l':[]) -> Scall e SynL
				[] -> e)
		] s i

call Texpr_top s i =
	p_or [
		([Tspn, Tmark, Tspn], \(_:e:_:[]) -> e)
--		,([Texpr], \(e:[]) -> e)
		] s i


parse s = p_or [([Texpr_top, Eos], \vs -> vs!!0)] s 0

res = "parser_test"


