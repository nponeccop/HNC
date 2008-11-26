module Parser (P (..), Syntax (..), SynParams (..), SynMark(..), parse, res) where

-- import Hugs.Observe

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
	| Spair Syntax Syntax
	deriving (Eq, Show)

data P = P Int Int Syntax | N Int
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
	| Tsave_args2
	| Tmark
	| Tmarks
	| Eos
	deriving Show

p_and ((t:ts),f) vi vs s i m =
	case call t s i m of
		P m i1 s1 -> p_and (ts,f) (i1+vi) (s1:vs) s (i+i1) (max m (i + i1))
		N i2 -> N (max m i2)
p_and ([],f) vi vs s i m =
	P m vi (f (reverse vs))
	
p_or (o:os) s i m =
	case p_and o 0 [] s i m of
		P m i s -> P m i s
		N i2 -> p_or os s i (max i2 m)

p_or [] s i m =
	N (max i m)

call (Tc c) s i m| i < length s && c == s!!i = P (max i m) 1 (Sc c)
call (Tc c) s i m = N (max i m)
call Eos s i m| i == length s = P (max i m) 0 (Ss "")
call Eos s i m = N (max i m)
call Tc1 s i m =
	p_or (map (\x -> ([Tc x], \vs -> vs!!0)) "_abcdefghijklmnopqrstuvwxyz") s i m
call Tb s i m =
	p_or [
		([Tc '1', Tc 'b'], \(c1:c2:[]) -> Sb True)
		,([Tc '0', Tc 'b'], \(c1:c2:[]) -> Sb False)
		] s i m
call Tsp s i m =
	p_or [
		([Tc ' ', Tsp], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc ' '], \(Sc c:[]) -> Ss (c:""))
		,([Tc '\n'], \(Sc c:[]) -> Ss (c:""))
		] s i m
call Tspn s i m =
	p_or [
		([Tsp], \(Ss s:[]) -> Ss s)
		,([], \([]) -> Ss "")
		] s i m
call Tcs s i m =
	p_or [
		([Tc1, Tcs], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc1], \(Sc c:[]) -> Ss (c:""))
		] s i m
call Tn s i m =
	p_or (map (\x -> ([Tc x], \(Sc c:[]) -> Sn (read (c:"")))) "0123456789") s i m
call Tnpos s i m =
	p_or [
		([Tn, Tnpos], \(Sn n:Sn n2:[]) -> Sn (n2 + 10 * n))
		,([Tn], \(sn:[]) -> sn)
		] s i m
call Tnneg s i m =
	p_or [
		([Tc '-', Tnpos], \(_:Sn n:[]) -> Sn (-n))
		] s i m
call Tnum s i m =
	p_or [
		([Tnpos], \(sn:[]) -> sn)
		,([Tnneg], \(sn:[]) -> sn)
		] s i m
call Tstring s i m =
    p_or [
        ([Tc '\'', Tcs, Tc '\''], \(Sc c1:Ss sn:Sc c2:[]) -> Sstr sn)
		] s i m


call Tparams s i m =
	p_or [
		([Tsp,Tval,Tparams], \(_:v:Sl l:[]) -> Sl (v:l))
--		,([Tsp,Tval], \(_:v:[]) -> Sl (v:[]))
		,([Tc ',',Texpr], \(_:c:[]) -> Sl (c:[]))
		,([], \([]) -> Sl [])
		] s i m
call Tval s i m =
	p_or [
		([Tb], \(b:[]) -> b)
		,([Tnum], \(n:[]) -> n)
		,([Tstring], \(n:[]) -> n)
		,([Tcs,Tnpos], \(Ss s:Sn n:[]) -> Ss $ s++show n) -- create new token?
		,([Tcs], \(s:[]) -> s)
		,([Tc '(', Texpr_top, Tc ')'], \(_:e:_:[]) -> e)
		] s i m
call Texpr s i m =
	p_or [
--		([Tcall,Tsave_args], \(c:Sl w:[]) -> Scall c (SynS (map (\(Ss s) -> s) w)))
		([Tval,Tparams], \(v:Sl a:[]) ->
			case a of
				[] -> v
				_ -> Scall v (SynK a))
		] s i m
call Tsave_args s i m =
	p_or [
		([Tcs,Tc '*',Tsave_args], \(c:_:Sl l:[]) -> Sl (c:l))
		,([], \([]) -> Sl [])
		] s i m
call Tsave_args2 s i m =
	p_or [
		([Tc '*',Tcs,Tc ':',Texpr,Tsave_args2], \(_:c:_:e:Sl l:[]) -> Sl ((Spair c e):l))
		,([], \([]) -> Sl [])
		] s i m
call Tmarks s i m =
	p_or [
		([Tc 'r',Tc '!',Tmarks], \(c:_:Sl l:[]) -> Sl (Sc 'r':l))
		,([Tc 'l',Tc '!',Tmarks], \(c:_:Sl l:[]) -> Sl (Sc 'l':l))
		,([], \([]) -> Sl [])
		] s i m
call Tsave s i m =
	p_or [
		([Tsave_args,Texpr,Tsave_args2], \(Sl w:e:Sl l:[]) ->
			let ee =
				case l of
					[] -> e
					xs -> Scall (Scall e (SynS (map (\(Spair (Ss s) _) -> s) l))) (SynK (map (\(Spair _ e) -> e) l))
			in
			case w of
				[] -> ee
				xs -> Scall ee (SynS (map (\(Ss s) -> s) w)))
		] s i m
call Tmark s i m =
	p_or [
		([Tmarks,Tsave], \(Sl m:e:[]) ->
			case m of
				(Sc 'r':[]) -> Scall e (SynM [MarkR])
				(Sc 'l':[]) -> Scall e SynL
				[] -> e)
		] s i m

call Texpr_top s i m =
	p_or [
		([Tspn, Tmark, Tspn], \(_:e:_:[]) -> e)
--		,([Texpr], \(e:[]) -> e)
		] s i m


parse s = p_or [([Texpr_top, Eos], \vs -> vs!!0)] s 0 0

res = parse "sum (,2)"


