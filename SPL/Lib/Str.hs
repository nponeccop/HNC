{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SPL.Lib.Str where

import SPL.Types

lib =
	("str_concat", do_concat)
	:("str_reverse", do_reverse)
	:("str_eq", do_eq)
	:("str_num", do_num)
	:[]

do_concat (CStr s1:CStr s2:[]) e =
	CStr (s1++s2)

do_reverse (CStr s:[]) e =
	CStr (reverse s)

do_eq (CStr s1:CStr s2:[]) e =
	CBool (s1 == s2)

do_num (CNum n:[]) e =
	CStr $ show n

