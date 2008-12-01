module Main where

import SPL.Interpretator
import SPL.Top
import Data.Map as M hiding (map)
import System

data Cmd =
	Quit | Help | Type | TypeDef | Code | Expr

get_cmd line =
	if "\\q" == take 2 line then Quit
	else if "\\h" == take 2 line then Help
	else if "\\t " == take 3 line then Type
	else if "\\td " == take 4 line then TypeDef
	else if "\\c " == take 3 line then Code
	else Expr

revision = "$Revision$"

help = 
	"help\n"++
	"  interactive commands\n"++
  "    \\ - interpretator internal commands\n"++
  "     h - help\n"++
  "     t exp - type\n"++
  "     q - quit\n"++
  "  apply\n"++
  "    f p - apply p to f\n"++
  "    f,expr - f (expr)\n"++
  "    (v1*v2*...expr) - define function with vX parameters\n"++
  "    (expr*a1:v1*a2:v2...) - (a1*a2*...expr) v1 v2 ...\n"++
  "  markers\n"++
  "    (l!expr) - lazy, eval: (l!expr) go\n"++
  "    (r!expr1,_f,expr2) - save current lambda to _f\n"++
  "  base functions\n"++
	"    "++(foldr1 (\a b -> a++" "++b) $ M.keys SPL.Top.base)++"\n"++
	"  web\n"++
  "    wiki http://code.google.com/p/inv/w/"

help2 =
	"use spli <spl module> to run file\nspli for interactive mode"

title =
	"SPL r"++revision++"\n"++(take 0 $ repeat ' ')++"\\h - help"

exec_file f = do
	s <- readFile f
	case step s of
		SPL.Interpretator.P (t, r) -> putStrLn r
		SPL.Interpretator.N (i, r) -> putStrLn (r++"\nchar# "++show i)

tab i s =
	(take i (repeat ' '))++"  ^"++s

main_loop = do
	putStr "  "
	line <- getLine
	case get_cmd line of
		Quit -> return ()
		Help -> do putStrLn help; main_loop
		Type ->
			case get_type_of_expr $ drop 3 line of
				SPL.Interpretator.P (t, r) -> do putStrLn t; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn r; main_loop
		TypeDef ->
			case get_type_debug_of_expr $ drop 4 line of
				SPL.Interpretator.P (t, r) -> do putStrLn t; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn r; main_loop
		Code ->
			case get_code_of_expr $ drop 3 line of
				SPL.Interpretator.P (i, r) -> do putStrLn r; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn (tab i r); main_loop
		Expr ->
			case step line of
				SPL.Interpretator.P (i, r) -> do putStrLn r; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn (tab i r); main_loop

spli = do
	args <- getArgs
	case args of
		"--help":t -> putStrLn help2
		h:t -> exec_file h
		[] -> do putStrLn title; main_loop

main = spli


