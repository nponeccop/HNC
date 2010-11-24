module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC
import qualified Test.Main
main = defaultMainWithHooks uuagcUserHook { runTests = \_ _ _ _ -> Test.Main.main }
