
module Test.HnParser (tests, testSet) where

import Parser.Parser
import Test.ParserTest (parseString)
import Test.HUnit


rtt _in = TestCase $ case parseString program _in of
	Right _ -> return ()
	Left msg -> assertFailure $ show msg

tests = map rtt testSet

testSet =
	[
		-- нумералы, вызовы ffi-функции
		"main = incr 2"
		-- определение функции, 1 параметр
	,	"main x = incr x"
		-- несколько параметров
	,	"main x z = sum x z"
		-- перекрытие fqn-функции аргументом
	,	"main sum = incr sum"
		-- перекрытие fqn-функции статической локальной
		-- BROKEN не должно быть typedef local
	,	"main x = { elist a b = sum a b\nelist x x }"
		-- перекрытие fqn-функции локальной переменной
	,	"main x z = { head = incr z\nsum x head }"
		-- константные строки
	,	"main = \"aaa\""
		-- локальное замыкание c аргументом
	,	"main a b = { c i = sum i b\nincr (sum a (c a)) }"

--	,	"main a b = { c i x = sum b (sum x i)\nd i = sum i i\nincr (c a (d a)) }"
		-- локальная переменная
	,	"main x = { y = sum x x\nsum y y }"
		-- & перед указателями на функции (рекомендуется новым стандартом C++)
		-- биндеры при передаче локальных функций аргументами
		-- f x y a -> f(x, y, hn::bind(impl, &main_impl::a))
--	,	"main x z l = { a = incr z\ny z = less (sum x a) z\nfilter y l }"
		-- BROKEN & перед указателями на статические функции
	,	"main l = {\n\tf x = less 1 x\n\tfilter f l\n}"

	-- l*((f*(filter f l)) (x*less 1 x))
		-- перекрытие fqn-функции статической
	,	"main x = { head z a = z a x\nhead sum 5 }"
			-- статическая функция
		-- BROKEN не должно быть typedef local
	,	"main x = { o a b = sum a b\no x x }"
		-- polymorphic function
	,	"main l f = filter f l"
	-- polymorphic function without free variables
	,	"flip f = { flipped x y = f y x\nflipped }"
	,	"map f l = {\n\tg x y = join1 (f x) y\n\tfoldr g elist l }"
		-- parameter name conflicts with inferred type variable name
	,	"main a f = filter f a"
	]
