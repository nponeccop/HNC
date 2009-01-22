module SPL.Parser (P (..), Syntax (..), SynParams (..), SynMark(..), parse, res) where

--import Hugs.Observe

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
	| Tchar_string
	| Tspace
	| Tspace_not
	| Tspace_any
	| Tnewline
	| Tnewline_space
	| Tdigit
	| Tdigits
	| Tnum_pos
	| Tnum_neg
	| Tnum
	| Tstring_quoted
	| Tstring_quoted2
	| Tstring
	| Tstring2
	| Tval
	| Tval2
	| Tpair
	| Tcall
	| Texpr
	| Texpr_top
	| Tparams
	| Tsave
	| Tsave_top
	| Tsave_args
	| Twhere_args
	| Twhere_args2
	| Twhere_args_star
	| Twhere_args_new
	| Tset
	| Tset_top
	| Tmark
	| Tmark_top
	| Tmarks
	| Tstruct
	| Tstruct_args
	| Tstruct_args_star
	| Tstruct_args_new
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
	p_or (map (\x -> ([Tchar x], \vs -> vs!!0)) "_abcdefghijklmnopqrstuvwxyz. /0123456789")
call Tchar_string =
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
		,([Tchar ' '], \(Sc c i:[]) -> Ss (c:"") i)
		,([Tchar '\t'], \(Sc c i:[]) -> Ss (c:"") i)
		]
call Tspace_not =
	p_or [
		([Tspace], \(Ss s i:[]) -> Ss s i)
		,([], \([]) -> Ss "" 0)
		]
call Tspace_any =
	p_or [
		([Tnewline_space], \(s:[]) -> s)
		,([Tspace_not], \(s:[]) -> s)
		]
call Tnewline =
	p_or [
		([Tchar '\r', Tnewline], \(Sc c i:Ss s _:[]) -> Ss (c:s) i)
		,([Tchar '\n', Tnewline], \(Sc c i:_:[]) -> Ss (c:"") i)
		,([Tchar '\r'], \(Sc c i:[]) -> Ss (c:"") i)
		,([Tchar '\n'], \(Sc c i:[]) -> Ss (c:"") i)
		]
call Tnewline_space =
	p_or [
		([Tnewline, Tspace_not], \(Ss s i:_:[]) -> Ss s i)
	]
call Tstring2 =
	p_or [
		([Tchar_string, Tstring2], \(Sc c i:Ss s _:[]) -> Ss (c:s) i)
		,([Tchar_string], \(Sc c i:[]) -> Ss (c:"") i)
		]
call Tstring =
	p_or [
			([Tstring2, Tnum_pos], \(Ss s i:Sn n _:[]) -> Ss (s++show n) i)
			,([Tstring2], \(Ss s i:[]) -> Ss s i)
		]
call Tstring_quoted2 =
	p_or [
		([Tchar_any, Tstring_quoted2], \(Sc c i:Ss s _:[]) -> Ss (c:s) i)
		,([Tchar_any], \(Sc c i:[]) -> Ss (c:"") i)
		]
call Tdigit =
	p_or (map (\x -> ([Tchar x], \(Sc c i:[]) -> Sc c i)) "0123456789")
call Tdigits =
	p_or [
		([Tdigit, Tdigits], \(Sc n i:Ss n2 _:[]) -> Ss (n:n2) i)
		,([Tdigit], \(Sc n i:[]) -> Ss (n:"") i)
		]
call Tnum_pos =
	p_or [
		([Tdigits], \(Ss n i:[]) -> Sn (read n) i)
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
		([Tchar '\'', Tstring_quoted2, Tchar '\''], \(Sc c1 i:Ss sn _:Sc c2 _:[]) -> Sstr sn i)
		]
call Tstruct =
	p_or [
		([Tchar '{',Tspace_any,Tstruct_args,Tspace_any,Tchar '}'], \(Sc _ i:_:Sl l _:_:_:[]) -> Sstruct l i)
		]
call Tstruct_args =
	p_or [
		([Tstruct_args_new], \(l:[]) -> l)
		,([Tstruct_args_star], \(l:[]) -> l)
		,([], \([]) -> Sl [] 0)
	]
call Tstruct_args_star =
	p_or [
		([Tset,Tchar '*',Tstruct_args_star], \(s:_:Sl l _:[]) -> Sl (s:l) (get_i s))
		,([Tset], \(s:[]) -> Sl (s:[]) (get_i s))
	]
call Tstruct_args_new =
	p_or [
		([Tset_top,Tnewline_space,Tstruct_args_new], \(s:_:Sl l _:[]) -> Sl (s:l) (get_i s))
		,([Tset_top], \(s:[]) -> Sl (s:[]) (get_i s))
	]

call Tdots =
	p_or [
		([Tchar '.',Tstring,Tdots], \(_:v:Sl l _:[]) -> Sl (v:l) (get_i v))
		,([], \([]) -> Sl [] 0)
	]

call Tparams =
	p_or [
		([Tspace,Tval,Tparams], \(_:v:Sl l _:[]) -> Sl (v:l) (get_i v))
		,([Tspace_any,Tchar ',',Texpr], \(_:_:c:[]) -> Sl (c:[]) (get_i c))
		,([Tspace_any,Tchar '#',Texpr], \(_:Sc _ i:c:[]) -> Sl ((Scall c SynL i):[]) (get_i c))
		,([], \([]) -> Sl [] 0)
		]
call Tval2 =
	p_or [
		([Tb], \(b:[]) -> b)
		,([Tnum], \(n:[]) -> n)
		,([Tstring_quoted], \(n:[]) -> n)
--		,([Tstring,Tnum_pos], \(Ss s i:Sn n _:[]) -> Ss (s++show n) i) -- create new token?
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
		([Tchar '#',Tval,Tparams], \(Sc _ i:v:Sl a _:[]) ->
			case a of
				[] -> Scall v SynL i
				_ -> Scall (Scall v (SynK a) (get_i v)) SynL i)
		,([Tval,Tparams], \(v:Sl a _:[]) ->
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
call Tset_top =
	p_or [
		([Tstring,Tchar ':',Tmark], \(Ss n i:_:e:[]) -> Sset n e i)
	]
call Twhere_args2 =
	p_or [
		([Twhere_args_star], \(l:[]) -> l)
		,([], \([]) -> Sl [] 0)
		]
call Twhere_args =
	p_or [
		([Twhere_args_star], \(l:[]) -> l)
		,([Twhere_args_new], \(l:[]) -> l)
		,([], \([]) -> Sl [] 0)
		]
call Twhere_args_star =
	p_or [
		([Tchar '*',Tset,Twhere_args_star], \(_:s:Sl l _:[]) -> Sl (s:l) (get_i s))
		,([Tchar '*',Tset], \(_:s:[]) -> Sl (s:[]) (get_i s))
		]
call Twhere_args_new =
	p_or [
		([Tnewline_space,Tset_top,Twhere_args_new], \(_:s:Sl l _:[]) -> Sl (s:l) (get_i s))
		,([Tnewline_space,Tset_top], \(_:s:[]) -> Sl (s:[]) (get_i s))
		]
call Tmarks =
	p_or [
		([Tchar 'r',Tchar '!',Tmarks], \(c:_:Sl l _:[]) -> Sl (Sc 'r' (get_i c):l) (get_i c))
		,([Tchar 'l',Tchar '!',Tmarks], \(c:_:Sl l _:[]) -> Sl (Sc 'l' (get_i c):l) (get_i c))
		,([], \([]) -> Sl [] 0)
		]
call Tsave =
	p_or [
		([Tsave_args,Tspace_any,Texpr,Twhere_args2], \(Sl w i:_:e:Sl l _:[]) ->
			let ee =
				case l of
					[] -> e
					xs -> Scall e (SynW l) i
			in
			case w of
				[] -> ee
				xs -> Scall ee (SynS (map (\(Ss s _) -> s) w)) i)
		]
call Tsave_top =
	p_or [
		([Tsave_args,Tspace_any,Texpr,Twhere_args], \(Sl w i:_:e:Sl l _:[]) ->
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
call Tmark_top =
	p_or [
		([Tmarks,Tsave_top], \(Sl m i:e:[]) ->
			case m of
				(Sc 'r' _:[]) -> Scall e (SynM [MarkR]) i
				(Sc 'l' _:[]) -> Scall e SynL i
				[] -> e)
		]

call Texpr_top =
	p_or [
		([Tspace_any, Tmark_top, Tspace_any], \(_:e:_:[]) -> e)
--		,([Texpr], \(e:[]) -> e)
		]


parse s = p_or [([Texpr_top, Eos], \vs -> vs!!0)] s 0 0
parse2 s = p_or [([Tstring2, Eos], \vs -> vs!!0)] s 0 0

res = parse2 $ take 59 $ repeat 'z'


