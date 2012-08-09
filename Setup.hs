module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC hiding(uuagc)
import UU.UUAGC
main = defaultMainWithHooks $ uuagcLibUserHook uuagc
