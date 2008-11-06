module Main where

import Interpretator
import Top
import Data.Map as M hiding (map)
import System

data Cmd =
	Quit | Help | Type | Expr

get_cmd line =
	if "\\q" == take 2 line then Quit
	else if "\\h" == take 2 line then Help
	else if "\\t " == take 3 line then Type
	else Expr

revision = "210"

help = 
	"help\n"
++"  \\ - interpretator internal commands\n"
++"   h - help\n"
++"   t exp - type\n"
++"   q - quit\n"
++"  apply\n"
++"   f p - apply p to f\n"
++"   f,expr - f (expr)\n"
++"   (v1*v2*...expr) - define function with vX parameters\n"
++"   (expr*a1:v1*a2:v2...) - set named values\n"
++"  base functions\n"
++"   "++(foldr1 (\a b -> a++" "++b) $ M.keys Top.base)

help2 =
	"use spli <spl module> to run file\nspli for interactive mode"

title =
	"SPL r"++revision++"\n"++(take 0 $ repeat ' ')++"\\h - help"

exec_file f = do
	s <- readFile f
	case step s of
		Interpretator.P (t, r) -> putStrLn r
		Interpretator.N (t, r) -> putStrLn t

main_loop = do
	putStr "  "
	line <- getLine
	case get_cmd line of
		Quit -> return ()
		Help -> do putStrLn help; main_loop
		Type ->
			case step $ drop 3 line of
				Interpretator.P (t, r) -> do putStrLn t; main_loop
				Interpretator.N (t, r) -> do putStrLn t; main_loop
		Expr ->
			case step line of
				Interpretator.P (t, r) -> do putStrLn r; main_loop
				Interpretator.N (t, r) -> do putStrLn t; main_loop

spli = do
	args <- getArgs
	case args of
		"--help":t -> putStrLn help2
		h:t -> exec_file h
		[] -> do putStrLn title; main_loop

main = spli


