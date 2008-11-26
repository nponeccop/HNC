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

call (Tc c) = \s i m ->
	case i < length s && c == s!!i of
		True -> P (max i m) 1 (Sc c)
		False -> N (max i m)
call Eos = \s i m ->
	case i == length s of
		True -> P (max i m) 0 (Ss "")
		False -> N (max i m)
call Tc1 =
	p_or (map (\x -> ([Tc x], \vs -> vs!!0)) "_abcdefghijklmnopqrstuvwxyz")
call Tb =
	p_or [
		([Tc '1', Tc 'b'], \(c1:c2:[]) -> Sb True)
		,([Tc '0', Tc 'b'], \(c1:c2:[]) -> Sb False)
		]
call Tsp =
	p_or [
		([Tc ' ', Tsp], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc ' '], \(Sc c:[]) -> Ss (c:""))
		,([Tc '\n'], \(Sc c:[]) -> Ss (c:""))
		]
call Tspn =
	p_or [
		([Tsp], \(Ss s:[]) -> Ss s)
		,([], \([]) -> Ss "")
		]
call Tcs =
	p_or [
		([Tc1, Tcs], \(Sc c:Ss s:[]) -> Ss (c:s))
		,([Tc1], \(Sc c:[]) -> Ss (c:""))
		]
call Tn =
	p_or (map (\x -> ([Tc x], \(Sc c:[]) -> Sn (read (c:"")))) "0123456789")
call Tnpos =
	p_or [
		([Tn, Tnpos], \(Sn n:Sn n2:[]) -> Sn (n2 + 10 * n))
		,([Tn], \(sn:[]) -> sn)
		]
call Tnneg =
	p_or [
		([Tc '-', Tnpos], \(_:Sn n:[]) -> Sn (-n))
		]
call Tnum =
	p_or [
		([Tnpos], \(sn:[]) -> sn)
		,([Tnneg], \(sn:[]) -> sn)
		]
call Tstring =
    p_or [
        ([Tc '\'', Tcs, Tc '\''], \(Sc c1:Ss sn:Sc c2:[]) -> Sstr sn)
		]


call Tparams =
	p_or [
		([Tsp,Tval,Tparams], \(_:v:Sl l:[]) -> Sl (v:l))
--		,([Tsp,Tval], \(_:v:[]) -> Sl (v:[]))
		,([Tc ',',Texpr], \(_:c:[]) -> Sl (c:[]))
		,([], \([]) -> Sl [])
		]
call Tval =
	p_or [
		([Tb], \(b:[]) -> b)
		,([Tnum], \(n:[]) -> n)
		,([Tstring], \(n:[]) -> n)
		,([Tcs,Tnpos], \(Ss s:Sn n:[]) -> Ss $ s++show n) -- create new token?
		,([Tcs], \(s:[]) -> s)
		,([Tc '(', Texpr_top, Tc ')'], \(_:e:_:[]) -> e)
		]
call Texpr =
	p_or [
--		([Tcall,Tsave_args], \(c:Sl w:[]) -> Scall c (SynS (map (\(Ss s) -> s) w)))
		([Tval,Tparams], \(v:Sl a:[]) ->
			case a of
				[] -> v
				_ -> Scall v (SynK a))
		]
call Tsave_args =
	p_or [
		([Tcs,Tc '*',Tsave_args], \(c:_:Sl l:[]) -> Sl (c:l))
		,([], \([]) -> Sl [])
		]
call Tsave_args2 =
	p_or [
		([Tc '*',Tcs,Tc ':',Texpr,Tsave_args2], \(_:c:_:e:Sl l:[]) -> Sl ((Spair c e):l))
		,([], \([]) -> Sl [])
		]
call Tmarks =
	p_or [
		([Tc 'r',Tc '!',Tmarks], \(c:_:Sl l:[]) -> Sl (Sc 'r':l))
		,([Tc 'l',Tc '!',Tmarks], \(c:_:Sl l:[]) -> Sl (Sc 'l':l))
		,([], \([]) -> Sl [])
		]
call Tsave =
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
		]
call Tmark =
	p_or [
		([Tmarks,Tsave], \(Sl m:e:[]) ->
			case m of
				(Sc 'r':[]) -> Scall e (SynM [MarkR])
				(Sc 'l':[]) -> Scall e SynL
				[] -> e)
		]

call Texpr_top =
	p_or [
		([Tspn, Tmark, Tspn], \(_:e:_:[]) -> e)
--		,([Texpr], \(e:[]) -> e)
		]


parse s = p_or [([Texpr_top, Eos], \vs -> vs!!0)] s 0 0

res = parse "sum (,2)"


