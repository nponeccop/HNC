{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SPL.Lib.Bignum where

import SPL.Types

lib =
	("bn_bignum_of_int", do_bignum_of_int)
	:("bn_bignum_of_str", do_bignum_of_str)
	:("bn_sum", do_sum)
	:("bn_mod", do_mod)
	:("bn_div", do_div)
	:("bn_eq", do_eq)
	:("bn_lt", do_lt)
	:[]

do_bignum_of_int (CNum i:[]) e =
	CF $ toInteger i

do_bignum_of_str (CStr s:[]) e =
	CF $ ((read s)::Integer)

do_sum (CF i:CF i2:[]) e =
	CF $ i + i2

do_mod (CF i:CF i2:[]) e =
	CF $ mod i i2

do_div (CF i:CF i2:[]) e =
	CF $ div i i2

do_eq (CF i:CF i2:[]) e =
	CBool $ (==) i i2

do_lt (CF i:CF i2:[]) e =
	CBool $ (<) i i2

