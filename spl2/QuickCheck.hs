module Main where

import Test.QuickCheck
import SPL.Types
import SPL.Check3
import SPL.Top

import SPL.Visualise
	
instance Arbitrary SPL.Types.C where
	arbitrary = sized $ \sz -> oneof [arb_cnum [] sz, arb_cbool [] sz, arb_sum sz] where
		-- произвольные выражения типа CNum
		arb_cnum l 0 = oneof $ (return $ CNum 2) : l
		arb_cnum _ sz = oneof $ [arb_sum (sz - 1), arb_fooInt (sz - 1)]

		arb_cbool l 0 = oneof $ (return $ CBool True) : l
		arb_cbool _ sz = oneof $ [arb_less (sz - 1), arb_fooBool (sz - 1)]

 		arb_sum sz = arb_binaryFunc sz f (arb_cnum []) (arb_cnum []) where
			f = \x y -> CL (CVal "sum") $ K [x, y]
		arb_less sz = arb_binaryFunc sz f (arb_cnum []) (arb_cnum []) where
			f = \x y -> CL (CVal "less") $ K [x, y]
		arb_fooInt sz = do
			x <- return $ CVal "foo"
			ysz <- choose (0, sz)
			let zsz = sz - ysz
			y <- arb_cnum [return $ CVal "foo"] (ysz + 1)
			z <- arb_cnum [] zsz
			return $ CL y $ W [("foo", z)]
		arb_fooBool sz = do
			x <- return $ CVal "foo"
			y <- arb_cbool [return $ CVal "foo"] sz
			z <- arb_cbool [] sz
			return $ CL y $ W [("foo", z)]

		
		arb_binaryFunc sz f arg1 arg2 = do
			arg1sz <- choose (0, sz)
			a1 <- arg1 arg1sz
			a2 <- arg2 (sz - arg1sz)
			return $ f a1 a2

data Foo = Foo C

instance Show Foo where
	show (Foo x) = show x ++ "\n\n" ++ showAsSource x

instance Arbitrary Foo where
	arbitrary = arbitrary >>= return . Foo 

check xs = case SPL.Top.check2 xs of
	P _ -> True
	_ -> False
		
prop_Foo (Foo xs) = (length $ show xs) < 500 ==> check xs

main = Test.QuickCheck.check (defaultConfig { configMaxTest = 500}) prop_Foo
