module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC
import qualified QuickCheck
main = defaultMainWithHooks uuagcUserHook { runTests = foo }

foo _ _ _ _ = QuickCheck.main
