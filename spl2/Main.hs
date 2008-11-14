
module Main where

-- import Visualise
import Core
import ParserTest
import Parser2
import Utils
import qualified Data.Map as M
import MyTypeCheck
import TypeParser
import Intermediate

import Check3
import Types
import HN.SplExport
import CPP.TypeProducer
import Top


simpleParse prog = head $ fromRight $ parseProg prog

baseToTdi = M.map (const $ CppFqMethod "ff") base

tdi = DefinitionInherited {
	diLevel = 3,
	diSymTab = baseToTdi
	-- M.fromList [ ("f", CppFqMethod "ffi"),  ("g", CppFqMethod "ffi"), ("h", CppFqMethod "ffi")]
}
    
test1 = rt (dsCppDef . (sem_Definition tdi))

test2 = rt (getDefinitionFreeVars) 

rt f = mapM (print . f . simpleParse) testSet

testSet = 
	[
		-- нумералы, вызовы ffi-функции
		"main = incr 2"
		-- определение функции, 1 параметр
	,	"main x = incr x"
		-- несколько параметров
	,	"main x z = sum x z"
		-- статическая функция
		-- BROKEN не должно быть typedef local
	,	"main x = { o a b = sum a b\no x x }"
		-- перекрытие fqn-функции аргументом
	,	"main sum = incr sum"
		-- перекрытие fqn-функции статической локальной
		-- BROKEN не должно быть typedef local
	,	"main x = { elist a b = sum a b\nelist x x }" 
		-- перекрытие fqn-функции локальной переменной
	,	"main x z = { head = incr z\nsum x head }"
		-- константные строки
	,	"main x = \"aaa\""
		-- перекрытие fqn-функции статической 
	,	"main x = { head z a = z a x\nhead sum 5 }"
		-- локальное замыкание c аргументом
	,	"main a b = { c i = g i b\nf (c a (g a)) }"
 
	,	"main a b = { c i = g i b\nd i = g i i\nf (c a (d a)) }"
		-- локальная переменная
	,	"main x = { y = x x\ng y y }"
		-- & перед указателями на функции (рекомендуется новым стандартом C++)
		-- биндеры при передаче локальных функций аргументами
		-- f x y a -> f(x, y, hn::bind(impl, &main_impl::a)) 
	,	"main x z = { a = incr z\ny z = sum x z\nf x y a }"
		-- BROKEN & перед указателями на статические функции 
	,	"main l = { f x = less 1 x\nfilter f l }"
	
	-- l*((f*(filter f l)) (x*less 1 x)) 		
	
	]
	
defaultEnv = Env 1 $ M.fromList $ map (\(a, b) -> (a, simpleParse2 b)) [
		("head",  "(List 1) -> 1" )
	,	("plus1", "Int -> Int" )
	]

testCheck3 = mapM (print . check0 . convertExpr) [
		Constant (ConstInt 123),
		Atom "a",
		Application (Atom "sum") $ map (Constant . ConstInt) [1, 2],
		Application (Atom "incr") $ [Atom "x"]
	]
	
main = do
--	mapM (print . simpleParse2) $ [ "aaa", "aaa bbb", "aaa -> bbb", "(List 1) -> 1", "Int -> Int" ]
--	print defaultEnv 
	mapM (print . snd . (typeCheck defaultEnv)) [
--			Atom "l"
--		,	Atom "head"
--		,	Application (Atom "head") [Atom "l"]
		]  
	runTests
	test1
	testCheck3
	rt convertDef
	rt $ \x -> check0 (convertDef x)
--	test2
	print $ showType $ cppType $ T "num" 
	getLine
	return ()
