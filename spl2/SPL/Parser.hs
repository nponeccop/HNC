module SPL.Parser (P (..), Syntax (..), SynParams (..), SynMark(..), parse, res) where

-- import Hugs.Observe

data SynMark =
	MarkR
	deriving (Eq, Show)

data SynParams =
	SynK [Syntax]
	| SynS [[Char]]
	| SynW [Syntax]
	| SynL
	| SynM [SynMark]
	deriving (Eq, Show)

data Syntax =
	Sc Char Int
	| Sb Bool Int
	| Ss [Char] Int
	| Sstr [Char] Int
	| Sn Int Int
	| Sl [Syntax] Int
	| Sval [Char] Int
	| Scall Syntax SynParams Int
	| Sset [Char] Syntax Int
	| Spair Syntax Syntax Int
	| Sstruct [Syntax] Int
	| Sdot Syntax [Char] Int
	deriving (Eq, Show)

get_i (Sc _ i) = i
get_i (Sb _ i) = i
get_i (Ss _ i) = i
get_i (Sstr _ i) = i
get_i (Sn _ i) = i
get_i (Sl _ i) = i
get_i (Sval _ i) = i
get_i (Scall _ _ i) = i
get_i (Spair _ _ i) = i

data P = P Int Int Syntax | N Int
	deriving (Eq, Show)

data Token =
	Ts [Char]
	| Tchar Char
	| Tb
	| Tchar_any
	| Tspace
	| Tspace_not
	| Tsnew
	| Tdigit
	| Tnum_pos
	| Tnum_neg
	| Tnum
	| Tstring_quoted
	| Tstring
	| Tval
	| Tval2
	| Tpair
	| Tcall
	| Texpr
	| Texpr_top
	| Tparams
	| Tsave
	| Tsave_args
	| Twhere_args
	| Tset
	| Tmark
	| Tmarks
	| Tstruct
	| Tdots
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

call (Tchar c) = \s i m ->
	case i < length s && c == s!!i of
		True -> P (max i m) 1 (Sc c i)
		False -> N (max i m)
call (Ts "") = \s i m ->
	P (max i m) 0 (Ss "" i)
call (Ts s) =
	p_or [
			((map (Tchar) s), \(Sc c i:l) -> Ss s i)
		]
call Eos = \s i m ->
	case i == length s of
		True -> P (max i m) 0 (Ss "" i)
		False -> N (max i m)
call Tchar_any =
	p_or (map (\x -> ([Tchar x], \vs -> vs!!0)) "_abcdefghijklmnopqrstuvwxyz")
call Tb =
	p_or [
		([Tchar '1', Tchar 'b'], \(c1:c2:[]) -> Sb True (get_i c1))
		,([Tchar '0', Tchar 'b'], \(c1:c2:[]) -> Sb False (get_i c1))
		]
call Tspace =
	p_or [
		([Tchar ' ', Tspace], \(Sc c i:Ss s _:[]) -> Ss (c:s) i)
		,([Tchar '\t', Tspace], \(Sc c i:_:[]) -> Ss (c:"") i)
		,([Tchar '\r', Tspace], \(Sc c i:_:[]) -> Ss (c:"") i)
		,([Tchar '\n', Tspace], \(Sc c i:_:[]) -> Ss (c:"") i)
		,([Tchar ' '], \(Sc c i:[]) -> Ss (c:"") i)
		,([Tchar '\t'], \(Sc c i:[]) -> Ss (c:"") i)
		,([Tchar '\r'], \(Sc c i:[]) -> Ss (c:"") i)
		,([Tchar '\n'], \(Sc c i:[]) -> Ss (c:"") i)
		]
call Tspace_not =
	p_or [
		([Tspace], \(Ss s i:[]) -> Ss s i)
		,([], \([]) -> Ss "" 0)
		]
call Tsnew =
	p_or [
		([Tspace_not,Tchar '\r',Tchar '\n',Tspace_not], \(Ss s i:_:_:_:[]) -> Ss s i)
		,([Tspace_not,Tchar '\r',Tspace_not], \(Ss s i:_:_:[]) -> Ss s i)
		,([Tspace_not,Tchar '\n',Tspace_not], \(Ss s i:_:_:[]) -> Ss s i)
		]
call Tstring =
	p_or [
		([Tchar_any, Tstring], \(Sc c i:Ss s _:[]) -> Ss (c:s) i)
		,([Tchar_any], \(Sc c i:[]) -> Ss (c:"") i)
		]
call Tdigit =
	p_or (map (\x -> ([Tchar x], \(Sc c i:[]) -> Sn (read (c:"")) i)) "0123456789")
call Tnum_pos =
	p_or [
		([Tdigit, Tnum_pos], \(Sn n i:Sn n2 _:[]) -> Sn (n2 + 10 * n) i)
		,([Tdigit], \(sn:[]) -> sn)
		]
call Tnum_neg =
	p_or [
		([Tchar '-', Tnum_pos], \(_:Sn n i:[]) -> Sn (-n) i)
		]
call Tnum =
	p_or [
		([Tnum_pos], \(sn:[]) -> sn)
		,([Tnum_neg], \(sn:[]) -> sn)
		]
call Tstring_quoted =
	p_or [
		([Tchar '\'', Tstring, Tchar '\''], \(Sc c1 i:Ss sn _:Sc c2 _:[]) -> Sstr sn i)
		]
call Tstruct =
	p_or [
		([Tchar '{', Tchar '}'], \(Sc _ i:_:[]) -> Sstruct [] i)
--		,([Tchar '{', Tset, Tchar '}'], \(Sc _ i:a:_:[]) -> Sstruct (a:[]) i)
		,([Tchar '{', Tspace_not, Tset, Twhere_args, Tspace_not, Tchar '}'], \(Sc _ i:_:a:Sl l _:_:_:[]) -> Sstruct (a:l) i)
		]
	

call Tdots =
	p_or [
		([Tchar '.',Tstring,Tdots], \(_:v:Sl l _:[]) -> Sl (v:l) (get_i v))
		,([], \([]) -> Sl [] 0)
	]

call Tparams =
	p_or [
		([Tspace,Tval,Tparams], \(_:v:Sl l _:[]) -> Sl (v:l) (get_i v))
		,([Tchar ',',Texpr], \(_:c:[]) -> Sl (c:[]) (get_i c))
		,([], \([]) -> Sl [] 0)
		]
call Tval2 =
	p_or [
		([Tb], \(b:[]) -> b)
		,([Tnum], \(n:[]) -> n)
		,([Tstring_quoted], \(n:[]) -> n)
		,([Tstring,Tnum_pos], \(Ss s i:Sn n _:[]) -> Ss (s++show n) i) -- create new token?
		,([Tstring], \(s:[]) -> s)
		,([Tstruct], \(s:[]) -> s)
		,([Tchar '(', Texpr_top, Tchar ')'], \(_:e:_:[]) -> e)
		]

call Tval =
	p_or [
		([Tval2,Tdots], \(v:Sl l _:[]) ->
			case l of
				[] -> v
				l2 -> foldl (\a (Ss b i) -> Sdot a b i) v l2)
	]

call Texpr =
	p_or [
		([Tval,Tparams], \(v:Sl a _:[]) ->
			case a of
				[] -> v
				_ -> Scall v (SynK a) (get_i v))
		]
call Tsave_args =
	p_or [
		([Tstring,Tchar '*',Tsave_args], \(c:_:Sl l _:[]) -> Sl (c:l) (get_i c))
		,([], \([]) -> Sl [] 0)
		]
call Tset =
	p_or [
		([Tstring,Tchar ':',Texpr], \(Ss n i:_:e:[]) -> Sset n e i)
	]
call Twhere_args =
	p_or [
		([Tchar '*',Tset,Twhere_args], \(_:s:Sl l _:[]) -> Sl (s:l) (get_i s))
		,([Tsnew,Tset,Twhere_args], \(_:s:Sl l _:[]) -> Sl (s:l) (get_i s))
		,([], \([]) -> Sl [] 0)
		]
call Tmarks =
	p_or [
		([Tchar 'r',Tchar '!',Tmarks], \(c:_:Sl l _:[]) -> Sl (Sc 'r' (get_i c):l) (get_i c))
		,([Tchar 'l',Tchar '!',Tmarks], \(c:_:Sl l _:[]) -> Sl (Sc 'l' (get_i c):l) (get_i c))
		,([], \([]) -> Sl [] 0)
		]
call Tsave =
	p_or [
		([Tsave_args,Texpr,Twhere_args], \(Sl w i:e:Sl l _:[]) ->
			let ee =
				case l of
					[] -> e
					xs -> Scall e (SynW l) i
			in
			case w of
				[] -> ee
				xs -> Scall ee (SynS (map (\(Ss s _) -> s) w)) i)
		]
call Tmark =
	p_or [
		([Tmarks,Tsave], \(Sl m i:e:[]) ->
			case m of
				(Sc 'r' _:[]) -> Scall e (SynM [MarkR]) i
				(Sc 'l' _:[]) -> Scall e SynL i
				[] -> e)
		]

call Texpr_top =
	p_or [
		([Tspace_not, Tmark, Tspace_not], \(_:e:_:[]) -> e)
--		,([Texpr], \(e:[]) -> e)
		]


parse s = p_or [([Texpr_top, Eos], \vs -> vs!!0)] s 0 0

res = parse "{a:1*b:2}"


