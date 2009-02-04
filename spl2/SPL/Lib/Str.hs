module SPL.Lib.Str where

import SPL.Types

lib =
	("str_concat", do_concat)
	:[]

do_concat (CStr s1:CStr s2:[]) e =
	CStr (s1++s2)

