module Main where

import qualified Data.Map as M

import HN.Parser2
import CPP.Core
import Utils
import CPP.Intermediate
import SPL.Top

simpleParse prog = head $ fromRight $ parseProg prog

tdi = DefinitionInherited {
	diLevel        = 0,
	diSymTab       = M.map (const $ CppFqMethod "ff") SPL.Top.get_types,
	diFreeVarTypes = SPL.Top.get_types
,	diType         = Nothing
}
    
main = do
	a <- readFile "test.hn"
	print $ dsCppDef $ sem_Definition tdi $ simpleParse a