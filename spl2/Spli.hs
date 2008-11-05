module Main where

import Interpretator
import Top
import Data.Map as M hiding (map)

data Cmd =
	Quit | Help | Type | Expr

get_cmd line =
	if "\\q" == take 2 line then Quit
	else if "\\h" == take 2 line then Help
	else if "\\t " == take 3 line then Type
	else Expr

help = 
	"help\n  \\ - interpretator internal commands\n   h - help\n   t exp - type\n   q - quit\n\n  base functions\n"++(foldr (\a b -> "   "++a++"\n"++b) "" $ M.keys Top.base)

mainLoop = do
	putStr "  "
	line <- getLine
	case get_cmd line of
		Quit -> return ()
		Help -> do putStrLn help; mainLoop
		Type ->
			case step $ drop 3 line of
				Interpretator.P (t, r) -> do putStrLn t; mainLoop
				Interpretator.N (t, r) -> do putStrLn t; mainLoop
		Expr ->
			case step line of
				Interpretator.P (t, r) -> do putStrLn r; mainLoop
				Interpretator.N (t, r) -> do putStrLn t; mainLoop

spli = do
	putStrLn ("SPL r200\n"++(take 20 $ repeat ' ')++"\\h - help")
	mainLoop

main = spli


