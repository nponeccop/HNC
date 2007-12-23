module Parser (P (..), Syntax (..), SynParams (..), SynMark(..), parse, res) where

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

data P = P Int Syntax | N
	deriving (Eq, Show)

data Token =
	Ts [Char]
	| Tc Char
	| Tb
	| Tc1
	| Tsp
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
call Eos s i = N


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
		([Tc '*',Tcs,Tsave_args], \(_:c:Sl l:[]) -> Sl (c:l))
		,([], \([]) -> Sl [])
--		,([Tc '*',Tcs], \(_:c:[]) -> Sl [c])
		] s i
call Tmarks s i =
	p_or [
		([Tc '!',Tc 'r',Tmarks], \(_:c:Sl l:[]) -> Sl (Sc 'r':l))
		,([Tc '!',Tc 'l',Tmarks], \(_:c:Sl l:[]) -> Sl (Sc 'l':l))
		,([], \([]) -> Sl [])
		] s i
call Tsave s i =
	p_or [
		([Texpr,Tsave_args], \(e:Sl w:[]) ->
			case w of
				[] -> e
				x:xs -> Scall e (SynS (map (\(Ss s) -> s) w)))
		] s i
call Tmark s i =
	p_or [
		([Tsave,Tmarks], \(e:Sl m:[]) ->
			case m of
				(Sc 'r':[]) -> Scall e (SynM [MarkR])
				(Sc 'l':[]) -> Scall e SynL
				[] -> e)
		] s i

call Texpr_top s i =
	p_or [
		([Tmark], \(e:[]) -> e)
--		,([Texpr], \(e:[]) -> e)
		] s i


parse s = p_or [([Texpr_top, Eos], \vs -> vs!!0)] s 0

tests = [
	("2", Sn 2)
	,("((((2))))", Sn 2)
	,("12", Sn 12)
	,("sum", Ss "sum")
	,("sum one", Scall (Ss "sum") (SynK [Ss "one"]))
	,("sum 11 22", Scall (Ss "sum") (SynK [Sn 11, Sn 22]))
	,("sum 11,min 22 33", Scall (Ss "sum") (SynK [Sn 11, Scall (Ss "min") (SynK [Sn 22, Sn 33])]))
	,("incr,min 22 33", Scall (Ss "incr") (SynK [Scall (Ss "min") (SynK [Sn 22, Sn 33])]))
	,("((incr),(min (22) (33)))", Scall (Ss "incr") (SynK [Scall (Ss "min") (SynK [Sn 22, Sn 33])]))
	,("sum 1*a*b", Scall (Scall (Ss "sum") (SynK [Sn 1])) (SynS ["a", "b"]))
	,("(sum 1,min 22 z*a*b),min z*x*y", Scall (Scall (Scall (Scall (Ss "sum") (SynK [Sn 1,Scall (Ss "min") (SynK [Sn 22,Ss "z"])])) (SynS ["a","b"])) (SynK [Scall (Ss "min") (SynK [Ss "z"])])) (SynS ["x","y"]))
	,("(sum a b*a*b) 12 22", Scall (Scall (Scall (Ss "sum") (SynK [Ss "a", Ss "b"])) (SynS ["a", "b"])) (SynK [Sn 12, Sn 22]))
	,("(if (less _ 5) (sum _ (_r (sum _ 1))) (_)*_!r) 1", Scall ((Scall (Scall (Scall (Ss "if") (SynK [Scall (Ss "less") (SynK [Ss "_",Sn 5]),Scall (Ss "sum") (SynK [Ss "_",Scall (Ss "_r") (SynK [Scall (Ss "sum") (SynK [Ss "_",Sn 1])])]),Ss "_"])) (SynS ["_"]))) (SynM [MarkR])) (SynK [Sn 1]))
	,("(_,list 1 2 3 4 5*_) (if (is_empty _) (list) (join (_r,filter (le h) _) h,join (list h) (_r,filter (more h) _)*h*t) (head _) (tail _)*_!r)", (Scall (Scall (Scall (Ss "_") (SynK [Scall (Ss "list") (SynK [Sn 1,Sn 2,Sn 3,Sn 4,Sn 5])])) (SynS ["_"])) (SynK [Scall (Scall (Scall (Ss "if") (SynK [Scall (Ss "is_empty") (SynK [Ss "_"]),Ss "list",Scall (Scall (Ss "join") (SynK [Scall (Ss "_r") (SynK [Scall (Ss "filter") (SynK [Scall (Ss "le") (SynK [Ss "h"]),Ss "_"])]),Ss "h",Scall (Ss "join") (SynK [Scall (Ss "list") (SynK [Ss "h"]),Scall (Ss "_r") (SynK [Scall (Ss "filter") (SynK [Scall (Ss "more") (SynK [Ss "h"]),Ss "_"])])])])) (SynS ["h","t"]),Scall (Ss "head") (SynK [Ss "_"]),Scall (Ss "tail") (SynK [Ss "_"])])) (SynS ["_"])) (SynM [MarkR])])))
	]

mk_test (s, e) =
	(case parse s of
		P _ s2|e == s2 -> "ok - "
		P _ s2 -> "se"++"("++(show s2)++") - "
		N -> "pe - ") ++ s

res = foldr1 (\a b -> a++"\n"++b) $ map mk_test tests


