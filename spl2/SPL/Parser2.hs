module SPL.Parser2 (P (..), Syntax (..), SynParams (..), SynMark(..), parse, res) where

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

data P = P Int Int Syntax | N Int
	deriving (Eq, Show)

data Token =
	Eos
	| Tstring_char
	| Tletter
	| Tdigit
	| Tdigits
	| Tnum_pos
	| Tnum_neg
	| Tnum

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

call Eos = \s i m ->
	case i == length s of
		True -> P (max i m) 0 (Ss "" i)
		False -> N (max i m)
call Tstring_char =
	p_or (map (\x -> ([Tchar x], \vs -> vs!!0)) ("_., /0123456789[]\""++['a'..'z']++['A'..'Z']))
call Tletter =
	p_or (map (\x -> ([Tchar x], \vs -> vs!!0)) "_abcdefghijklmnopqrstuvwxyz")
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
call Tstring_chars =
	p_or [
		([Tchar_any, Tstring_chars], \(Sc c i:Ss s _:[]) -> Ss (c:s) i)
		,([Tchar_any], \(Sc c i:[]) -> Ss (c:"") i)
		]


