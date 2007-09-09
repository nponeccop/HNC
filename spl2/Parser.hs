module Parser where

import Code
import Debug.Trace

data SynParams =
	SynK [Syntax]
	| SynS [[Char]]
	deriving Show

data Syntax =
	Sc Char
	| Ss [Char]
	| Sn Int
	| Sl [Syntax]
	| Sval [Char]
	| Scall Syntax SynParams
	deriving Show

data P = P Int Syntax | N
	deriving Show

data Token =
	Ts [Char]
	| Tc Char
	| Tc1
	| Tsp
	| Tn
	| Tnpos
	| Tnneg
	| Tnum
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
	| Eos
	deriving Show

p_and ((t:ts),f) vi vs s i =
	case call t s i of
		P i1 s1 -> p_and (ts,f) (i1+vi) (s1:vs) s (i+i1)
		N -> N
p_and ([],f) vi vs s i =
	P vi (f (reverse vs))
	
p_or (o:os) s i =
	case p_and o 0 [] s i of
		P i s -> P i s
		N -> p_or os s i
p_or [] s i =
	N

call (Tc c) s i| i < length s && c == s!!i = P 1 (Sc c)
call (Tc c) s i = N
call Tc1 s i =
	p_or (map (\x -> ([Tc x], \vs -> vs!!0)) "abcdefghijklmnopqrstuvwxyz") s i
call Tsp s i =
	p_or [
		([Tc ' ', Tsp], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc ' '], \(Sc c:[]) -> Ss (c:""))
		] s i
call Tcs s i =
	p_or [
		([Tc1, Tcs], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc1], \(Sc c:[]) -> Ss (c:""))
		] s i
call Tn s i =
	p_or (map (\x -> ([Tc x], \(Sc c:[]) -> Sn (read (c:"")))) "012345679") s i
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
call Eos s i| i == length s = P 0 (Ss "")
call Eos s i = N


call Tparams s i =
	p_or [
		([Tsp,Tval,Tparams], \(_:v:Sl l:[]) -> Sl (v:l))
		,([Tsp,Tval], \(_:v:[]) -> Sl (v:[]))
		,([Tc ',',Texpr], \(_:c:[]) -> Sl (c:[]))
		] s i
call Tcall s i =
	p_or [
		([Tval,Tparams], \(v:Sl a:[]) -> Scall v (SynK a))
		] s i
call Tval s i =
	p_or [
		([Tnum], \(n:[]) -> n)
		,([Tcs], \(s:[]) -> s)
		,([Tc '(', Texpr_top, Tc ')'], \(_:e:_:[]) -> e)
		,([Tsave], \(e:[]) -> e)
		] s i
call Texpr s i =
	p_or [
		([Tcall], \(c:[]) -> c)
		,([Tval], \(c:[]) -> c)
		] s i
call Tsave_args s i =
	p_or [
		([Tc '\\',Tcs,Tsave_args], \(_:c:Sl l:[]) -> Sl (c:l))
		,([Tc '\\',Tcs], \(_:c:[]) -> Sl [c])
		] s i
call Tsave s i =
	p_or [
		([Texpr,Tsave_args], \(e:Sl w:[]) -> Scall e (SynS (map (\(Ss s) -> s) w)))
		] s i
call Texpr_top s i =
	p_or [
		([Tsave], \(e:[]) -> e)
		,([Texpr], \(e:[]) -> e)
		] s i


m = p_or [([Texpr_top, Eos], \vs -> vs!!0)] "sum 12 13 (14),sum 2 3,4\\a\\b 1 2" 0



