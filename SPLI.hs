module Main where

import SPL.Interpretator
import SPL.Top
import Data.Map as M hiding (map)
import System.Environment
import System.IO

data Cmd =
	Quit | Help | Examples | Type | TypeDef | TypeTree | Code | Expr | Syntax

get_cmd line =
	if "\\q" == take 2 line then Quit
	else if "\\h" == take 2 line then Help
	else if "\\e" == take 2 line then Examples
	else if "\\t " == take 3 line then Type
	else if "\\td " == take 4 line then TypeDef
	else if "\\tt " == take 4 line then TypeTree
	else if "\\c " == take 3 line then Code
	else if "\\s " == take 3 line then Syntax
	else Expr

revision = "$Revision$"

help = 
	"help\n"++
	"  interactive commands\n"++
  "    \\ - interpretator internal commands\n"++
  "     h - help\n"++
  "     e - examples\n"++
  "     t exp - type\n"++
  "     q - quit\n"++
  "  apply\n"++
  "    f p - apply p to f\n"++
  "    f,expr - f (expr)\n"++
  "    a1*a2*...expr - define function with aX parameters\n"++
  "    expr*a1:v1*a2:v2... - (a1*a2*...expr) v1 v2 ...\n"++
  "    a1*a2*expr*a3:v3*a4:v4 - both ^ and ^^\n"++
  "  lazy/rec\n"++
	"    f#expr - f {expr}\n"++
  "    {expr} - lazy, eval: {expr} go\n"++
  "    ('..._f p1 p2...) - save current lambda to _f (recursion)\n"++
  "  base functions\n"++
	"    "++(foldr1 (\a b -> a++" "++b) $ M.keys SPL.Top.base)++"\n"++
	"  structures\n"++
	"    struct.field - field from structure\n"++
	"    struct^field - import struct env\n"++
	"    .field - from top level\n"++
	"  web\n"++
  "    wiki http://code.google.com/p/inv/w/"

examples =
	"  http://code.google.com/p/inv/wiki/Examples"

help2 =
	"use spli <spl module> to run file\nspli for interactive mode"

title =
	"SPL "++((:) 'r' $ reverse $ drop 2 $ reverse $ drop 11 $ revision)++"\n"++(take 0 $ repeat ' ')++"\\h - help"

get_line ('\r':'\n':cs) s i il l|i>0 = get_line cs "" (i-2) 0 (l+1)
get_line ('\r':'\n':cs) s i il l = (l, il, s)
get_line ('\n':'\r':cs) s i il l|i>0 = get_line cs "" (i-2) 0 (l+1)
get_line ('\n':'\r':cs) s i il l = (l, il, s)
get_line ('\r':cs) s i il l|i>0 = get_line cs "" (i-1) 0 (l+1)
get_line ('\r':cs) s i il l = (l, il, s)
get_line ('\n':cs) s i il l|i>0 = get_line cs "" (i-1) 0 (l+1)
get_line ('\n':cs) s i il l = (l, il, s)
get_line ('\t':cs) s i il l|i>0 = get_line cs (s++"    ") (i-1) (il+4) l
get_line (c:cs) s i il l|i>0 = get_line cs (s++[c]) (i-1) (il+1) l
get_line (c:cs) s i il l = get_line cs (s++[c]) (i-1) (il+1) l
get_line [] s i il l = (l, il, s)

exec_file f = do
	s <- readFile f
	case step s of
		SPL.Interpretator.P (t, r) -> putStrLn r
		SPL.Interpretator.N (i, r) ->
			let (line, il, err_line) = get_line s "" i 0 1 in
				putStrLn (r++"\nline# "++show line++"  char# "++show i++"\n"++err_line++"\n"++(take il $ repeat ' ')++"^ "++r)

tab i s =
	(take i (repeat ' '))++"  ^"++s

main_loop = do
	putStr "  ";
	hFlush stdout;
	line <- getLine
	case get_cmd line of
		Quit -> return ()
		Help -> do putStrLn help; main_loop
		Examples -> do putStrLn examples; main_loop
		Type ->
			case get_type_of_expr $ drop 3 line of
				SPL.Interpretator.P (t, r) -> do putStrLn t; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn r; main_loop
		TypeDef ->
			case get_type_debug_of_expr $ drop 4 line of
				SPL.Interpretator.P (t, r) -> do putStrLn t; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn r; main_loop
		TypeTree ->
			case get_type_tree_of_expr $ drop 4 line of
				SPL.Interpretator.P (t, r) -> do putStrLn t; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn r; main_loop
		Code ->
			case get_code_of_expr $ drop 3 line of
				SPL.Interpretator.P (t, r) -> do putStrLn r; main_loop
				SPL.Interpretator.N (i, r) -> do putStrLn (tab i r); main_loop
		Syntax ->
			case get_syntax_of_expr $ drop 3 line of
				SPL.Interpretator.P (t, r) -> do putStrLn t; main_loop
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


