module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC

main = defaultMainWithHooks uuagcUserHook
