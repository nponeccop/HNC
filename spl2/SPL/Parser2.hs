module SPL.Parser2 (P (..), Syntax (..), SynParams (..), parse, res) where

import Data.Map as M hiding (map)
import System.IO.Unsafe

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

data P = P Int Int Syntax (M.Map (Token,Int) (Either (Int,Int,Syntax) Int)) | N Int (M.Map (Token,Int) (Either (Int,Int,Syntax) Int))
	deriving (Eq, Show)

data Token =
	Eos
  | Tchar Char
	| Tstring_char
	| Tstring_chars
	| Tletter
	| Tletters
	| Tstring
	| Tdigit
	| Tdigits
	| Tnum_pos
	| Tnum_neg
	| Tnum
	| Tbool
	| Tvar
	| Tval_simple
	| Tval_sep
	| Tval
	| Tspace
	| Tspace_o_not
	| Tnewline_space
	| Tspace_any
	| Tparams
	| Texpr
	| Texpr_top
	| Texpr_lambda
	| Tlambda
	| Tset
	| Twhere
	| Tstruct
	| Tkeys
	deriving (Eq, Show, Ord)

out o =
	unsafePerformIO $
	do
		putStrLn (show o);
		return o

out3 t i o =
	unsafePerformIO $
	do
		case t of
			Texpr_top -> do putStr ("top:"++show i++",");
--			Tval_simple -> do putStr ("vs:"++show i++",");
--			Tvar -> do putStr ("var:"++show i++",");
--			Tkeys -> do putStr ("keys:"++show i++",");
			_ -> do return ()
		return o

get_i (Sc _ i) = i
get_i (Sb _ i) = i
get_i (Ss _ i) = i
get_i (Sstr _ i) = i
get_i (Sn _ i) = i
get_i (Sl _ i) = i
get_i (Sval _ i) = i
get_i (Scall _ _ i) = i
get_i (Spair _ _ i) = i
get_i (Sdot _ _ i) = i
get_i o = error ("get_i: "++show o)

p_and ((t:ts),f) vi vs s i m c =
	case M.lookup (t,i) c of
		Just v ->
			case v of
				Left (m,i1,s1) -> p_and (ts,f) (i1+vi) (s1:vs) s (i+i1) (max m (i + i1)) c
				Right i2 -> N i2 c
		Nothing ->
			case call t s i m c of
				P m i1 s1 c -> p_and (ts,f) (i1+vi) (s1:vs) s (i+i1) (max m (i + i1)) (M.insert (t,i) (Left (m,i1,s1)) c)
				N i2 c -> N (max m i2) (M.insert (t,i) (Right i2) c)

p_and ([],f) vi vs s i m c =
	P m vi (f (reverse vs)) c
	
p_or (o:os) s i m c =
	case p_and o 0 [] s i m c of
		P m i s c -> P m i s c
		N i2 c -> p_or os s i (max i2 m) c

p_or [] s i m c =
	N (max i m) c

call Eos = \s i m c ->
	case i == length s of
		True -> P (max i m) 0 (Ss "" i) c
		False -> N (max i m) c
call (Tchar c) = \s i m c2 ->
	case i < length s && c == s!!i of
		True -> P (max i m) 1 (Sc c i) c2
		False -> N (max i m) c2
call Tstring_char = \s i m c ->
	case i < length s && elem (s!!i) ("_., /0123456789[]\""++['a'..'z']++['A'..'Z']) of
		True -> P (max i m) 1 (Sc (s!!i) i) c
		False -> N (max i m) c
call Tletter = \s i m c ->
	case i < length s && elem (s!!i) "_abcdefghijklmnopqrstuvwxyz" of
		True -> P (max i m) 1 (Sc (s!!i) i) c
		False -> N (max i m) c
call Tletters =
	p_or [
		([Tletter, Tletters], \(Sc n i:Ss n2 _:[]) -> Ss (n:n2) i)
		,([Tletter], \(Sc n i:[]) -> Ss (n:"") i)
		]
call Tdigit = \s i m c ->
	case i < length s && elem (s!!i) ("0123456789") of
		True -> P (max i m) 1 (Sc (s!!i) i) c
		False -> N (max i m) c
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
call Tbool =
	p_or [
		([Tchar '1', Tchar 'b'], \(Sc _ i:_:[]) -> Sb True i)
		,([Tchar '0', Tchar 'b'], \(Sc _ i:_:[]) -> Sb False i)
		]
call Tstring_chars =
	p_or [
		([Tstring_char, Tstring_chars], \(Sc c i:Ss s _:[]) -> Ss (c:s) i)
		,([Tstring_char], \(Sc c i:[]) -> Ss (c:"") i)
		]
call Tstring =
	p_or [
		([Tchar '\'', Tstring_chars, Tchar '\''], \(Sc _ i:Ss s _:_:[]) -> Sstr s i)
		]
call Tvar =
	p_or [
		([Tletters,Tnum_pos], \(Ss s i:Sn n _:[]) -> Ss (s++show n) i)
		,([Tletters], \(Ss s i:[]) -> Ss s i)
		]
call Tval_simple =
	p_or [
		([Tvar], \(a:[]) -> a)
		,([Tbool], \(b:[]) -> b)
		,([Tnum], \(n:[]) -> n)
		,([Tval_sep], \(n:[]) -> n)
		]
call Tval_sep =
	p_or [
		([Tstring], \(s:[]) -> s)
		,([Tchar '{',Tspace_any,Texpr_top,Tspace_any,Tchar '}'], \(Sc _ i:_:e:_:_:[]) -> Scall e SynL i)
		,([Tchar '(',Tchar '\'',Tspace_any,Texpr_top,Tspace_any,Tchar ')'], \(Sc _ i:_:_:e:_:_:[]) -> Scall e (SynM [MarkR]) i)
		,([Tchar '(',Tspace_any,Texpr_top,Tspace_any,Tchar ')'], \(Sc _ i:_:e:_:_:[]) -> e)
		,([Tstruct], \(s:[]) -> s)
		]
call Tkeys =
	p_or [
		([Tchar '.',Tvar,Tkeys], \(c:v:Sl l _:[]) -> Sl (v:l) (get_i c))
		,([Tchar '.',Tvar], \(c:v:[]) -> Sl (v:[]) (get_i c))
		]
call Tval =
	p_or [
		([Tval_simple,Tkeys], \(v:Sl l _:[]) -> foldl (\v (Ss k i) -> Sdot v k i) v l)
		,([Tval_simple], \(v:[]) -> v)
		]
call Tspace =
	p_or [
		([Tchar ' ',Tspace], \_ -> Ss "" 0)
		,([Tchar '\t',Tspace], \_ -> Ss "" 0)
		,([Tchar ' '], \_ -> Ss "" 0)
		,([Tchar '\t'], \_ -> Ss "" 0)
		]
call Tspace_o_not =
	p_or [
		([Tspace], \_ -> Ss "" 0)
		,([], \_ -> Ss "" 0)
		]
call Tnewline_space =
	p_or [
		([Tchar '\r',Tchar '\n',Tspace_o_not], \_ -> Ss "" 0)
		,([Tchar '\n',Tspace_o_not], \_ -> Ss "" 0)
		]
call Tspace_any =
	p_or [
		([Tnewline_space], \_ -> Ss "" 0)
		,([Tspace_o_not], \_ -> Ss "" 0)
		]
call Tparams =
	p_or [
		([Tspace_any,Tchar ',',Texpr], \(_:_:e:[]) -> Sl (e:[]) (get_i e))
		,([Tspace_any,Tchar '#',Texpr], \(_:Sc _ i:e:[]) -> Sl ((Scall e SynL i):[]) i)
		,([Tspace,Tval,Tparams], \(_:v:Sl l _:[]) -> Sl (v:l) (get_i v))
		,([Tspace,Tval], \(_:v:[]) -> Sl (v:[]) (get_i v))
		,([Tval_sep,Tparams], \(v:Sl l _:[]) -> Sl (v:l) (get_i v))
		,([Tval_sep], \(v:[]) -> Sl (v:[]) (get_i v))
		]
call Texpr =
	p_or [
		([Tval, Tparams], \(v:Sl p _:[]) -> Scall v (SynK p) (get_i v))
		,([Tval], \(v:[]) -> v)
		]
call Tlambda =
	p_or [
			([Tvar, Tchar '*',Tlambda], \(v:_:Sl l _:[]) -> Sl (v:l) (get_i v))
			,([Tvar, Tchar '*'], \(v:_:[]) -> Sl (v:[]) (get_i v))
		]
call Tset =
	p_or [
			([Tnewline_space, Tvar, Tchar ':', Texpr_lambda], \(Ss _ i:Ss n _:_:e:[]) -> Sset n e i)
			,([Tspace_o_not,Tchar '*', Tvar, Tchar ':', Texpr_lambda], \(_:Sc _ i:Ss n _:_:e:[]) -> Sset n e i)
		]
call Twhere =
	p_or [
			([Tset, Twhere], \(s:Sl l _:[]) -> Sl (s:l) (get_i s))
			,([Tset], \(s:[]) -> Sl (s:[]) (get_i s))
		]
call Tstruct =
	p_or [
			([Tchar '[',Twhere,Tspace_o_not,Tchar ']'], \(c:Sl l _:_:_:[]) -> Sstruct l (get_i c))
			,([Tchar '[',Twhere,Tnewline_space,Tchar ']'], \(c:Sl l _:_:_:[]) -> Sstruct l (get_i c))
		]
call Texpr_lambda =
	p_or [
		([Tlambda,Texpr], \(Sl l i:e:[]) -> Scall e (SynS $ map (\(Ss s _) -> s) l) i)
		,([Texpr], \(e:[]) -> e)
		]
call Texpr_top =
	p_or [
		([Tlambda,Tspace_any,Texpr,Twhere], \(Sl l i:_:e:Sl w _:[]) -> Scall (Scall e (SynW w) i) (SynS $ map (\(Ss s _) -> s) l) i)
		,([Tlambda,Tspace_any,Texpr], \(Sl l i:_:e:[]) -> Scall e (SynS $ map (\(Ss s _) -> s) l) i)
		,([Texpr,Twhere], \(e:Sl w _:[]) -> Scall e (SynW w) (get_i e))
		,([Texpr], \(e:[]) -> e)
		]

parse s = p_or [([Tspace_any,Texpr_top,Tspace_any,Tspace_any,Eos], \(_:e:_:_:_:[]) -> e)] s 0 0 M.empty

res = ""


