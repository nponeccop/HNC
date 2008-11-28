module Main where

import qualified Data.Map as M

import HN.Parser2
import CPP.Core
import Utils
import CPP.Intermediate
import SPL.Top

simpleParse prog = head $ fromRight $ parseProg prog

baseToTdi = M.map (const $ CppFqMethod "ff") SPL.Top.get_types

tdi = DefinitionInherited {
	diLevel        = 3,
	diSymTab       = baseToTdi,
	diFreeVarTypes = SPL.Top.get_types
,	diType         = Nothing
}
    
main = do
	a <- readFile "test.hn"
	print $ dsCppDef $ sem_Definition tdi $ simpleParse a