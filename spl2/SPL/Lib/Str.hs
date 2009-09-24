module SPL.Lib.Str where

import SPL.Types

lib =
	("str_concat", do_concat)
	:("str_reverse", do_reverse)
	:("str_eq", do_eq)
	:[]

do_concat (CStr s1:CStr s2:[]) e =
	CStr (s1++s2)

do_reverse (CStr s:[]) e =
	CStr (reverse $ show s)

do_eq (CStr s1:CStr s2:[]) e =
	CBool (s1 == s2)

