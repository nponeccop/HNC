
module Main where

import Visualise
import Core
import ParserTest
import Parser2
import Utils
import qualified Data.Map as M
import MyTypeCheck
import TypeParser
import Intermediate

import Check3
import Code

simpleParse prog = head $ fromRight $ parseProg prog

tdi = DefinitionInherited {
	diLevel = 3,
	diSymTab = M.fromList [ ("f", CppFqMethod "ffi"),  ("g", CppFqMethod "ffi"), ("h", CppFqMethod "ffi")]
}
    
test1 = rt (dsCppDef . (sem_Definition tdi))

test2 = rt (getDefinitionFreeVars) 

rt f = mapM (print . f . simpleParse) testSet

testSet = 
	[
		-- вызовы ffi-функции
		"main = f x"
		-- определение функции, 1 параметр
	,	"main x = f x"
		-- несколько параметров
	,	"main x z = f x y"
		-- статическая функция
		-- BROKEN не должно быть typedef local
	,	"main x = { o a b = g a a b\no x x }"
		-- перекрытие fqn-функции аргументом
	,	"main g = f g"
		-- перекрытие fqn-функции статической локальной
		-- BROKEN не должно быть typedef local
	,	"main x = { g z = z z\ng y y }"
		-- перекрытие fqn-функции локальной переменной
	,	"main x z = { y = b z\nf x y a }"
		-- константные строки
	,	"main x = \"aaa\""
			-- перекрытие fqn-функции статической 
	,	"main x = { g z a = z a x\ng y y }"
		-- локальное замыкание c аргументом
	,	"main a b = { c i = g i b\nf (c a (g a)) }"
 
	,	"main a b = { c i = g i b\nd i = g i i\nf (c a (d a)) }"
		-- локальная переменная
	,	"main x = { y = x x\ng y y }"
		-- & перед указателями на функции (рекомендуется новым стандартом C++)
		-- биндеры при передаче локальных функций аргументами
		-- f x y a -> f(x, y, hn::bind(impl, &main_impl::a)) 
	,	"main x z = { a = b z\ny z = g x z\nf x y a }"
		-- BROKEN & перед указателями на статические функции 
	,	"main l = { f x = g 1 x\nmap f l }" 		
	
	]
	
defaultEnv = Env 1 $ M.fromList $ map (\(a, b) -> (a, simpleParse2 b)) [
		("head",  "(List 1) -> 1" )
	,	("plus1", "Int -> Int" )
	]
	
convertExpr (Constant (ConstInt i)) = CNum 123
convertExpr (Atom a) = CVal a
convertExpr (Application a b) = 
	
testCheck3 = mapM (print . check0 . convertExpr) [
		Constant (ConstInt 123),
		Atom "a"
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
--	test2
	getLine
	return ()