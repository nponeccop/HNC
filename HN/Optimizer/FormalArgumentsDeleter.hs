{-# LANGUAGE GADTs, TypeFamilies, NoMonomorphismRestriction, FlexibleContexts #-}
module HN.Optimizer.FormalArgumentsDeleter (runB) where

import Compiler.Hoopl hiding ((<*>))
import qualified Control.Monad as CM
import qualified Data.Map as M
import Data.Monoid
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.ArgumentValues (ArgFact, argLattice, AFType(..))
import HN.Optimizer.Utils
import Utils
{-

Идея - использовать Hoopl как фреймворк произвольных оптимизаторов графов,
а не только "императивной лапши с Goto", как описано в статье

Инлайнинг:

Вход:

foo x = {
	bar x' = mul (incr x') x'
	bar (add x x)
}

Факты:

bar -> 1

Выход (без учета других оптимизаций):

foo x = {
	x' = add x x
    mul (incr x') x'
}

Основа Hoopl - таблица меток, содержащая факты - результаты анализа перехода на метку.

Попробую использовать таблицу символов, содержащую факты - результаты анализа

Для начала можно попробовать универсальные узлы без данных. В таком
виде можно представить любой граф. Конструктору узла передаются
два параметра: имя узла и имена узлов, к которым идут дуги.

Пока похоже, что составные блоки нужны только для императивных последовательностей инструкций

--data Node0 e x where
--	Node0 :: Label -> [Label] -> Node0 C С

-- для блоков из одного СС-узла нет вспомогательных функций

foo x = {
	bar x = incr x
	bar x
}

Основная идея обратного анализа:

На вход подается FactBase ListFact, полученный в результате прямого анализа,
в котором для функций, которые нужно инлайнить, указано Bot, а для функций,
которые не нужно инлайнить - Top.

Обратный анализатор для каждой функции возвращает [...]. Соответственно:

Top `join` [...] = Top
Bottom `join` [...] = [...]

При перезаписи тело функции движется назад от места определения к месту вызова. 

Фактические параметры движутся вперёд от места вызова к формальным параметрам, которые меняются
с ArgNode на LetNode.
-}

-- listLattice = addPoints' "ListFact" $ \_ (OldFact _) (NewFact new) -> error "Join" (SomeChange, PElem new)

-- BACKWARD pass
--

transferB' :: Node e x -> Fact x ArgFact -> ArgFact
transferB' (Entry l) o @ (curFact, factBase) = newFact where
	baseFact = M.lookup l factBase
	newFact = case baseFact of
		Nothing -> (curFact, M.insert l curFact factBase)
		Just baseFact -> case join (OldFact baseFact) (NewFact curFact) of
			Nothing -> (baseFact, factBase)
			Just newFact -> (newFact, M.insert l newFact factBase)

transferB' (Exit d) o = transferB d o

transferB :: DefinitionNode -> FactBase ArgFact -> ArgFact
transferB LibNode _ = ((Top, bot), bot)
transferB ArgNode _ = ((Top, bot), bot)
--transferB (LetNode [] _) _ = ((Top, bot), bot)
transferB (LetNode _ _) _ = bot

-- Rewriting: inline definition - rewrite Exit nodes ("call sites") to
-- remove references to the definition being inlined
--

rewriteB :: DefinitionNode -> FactBase ArgFact -> Maybe DefinitionNode
rewriteB xll f = case xll of
	LibNode -> Nothing
	ArgNode -> Nothing
	LetNode l expr -> LetNode l <$> process' (rewriteExpression $ mapMapWithKey convertFact $ xtrace "rewrite.f" f) expr

convertFact :: Label -> ArgFact -> Maybe [WithTopAndBot ExpressionFix]
convertFact l ((callFact, _), m) = case xtrace "callFact" callFact of
	PElem a -> Just a
	Top -> Nothing
	x -> case uncondLookup (xtrace "label" l) m of
		(PElem callFact, _) -> Just $ xtrace "callFact2" callFact
		_ -> Nothing

rewriteExpression f (Application aa @ (Atom a) b) = smartApplication aa <$> (rewriteArguments b $ CM.join $ lookupFact a f)
rewriteExpression _ _ = Nothing

rewriteArguments b f = map fst <$> (process deleteArg =<< zipExactNote "AD.rewriteArguments" b <$> f)

smartApplication a [] = a
smartApplication a b = Application a b

deleteArg :: Rewrite [(ExpressionFix, WithTopAndBot ExpressionFix)]
deleteArg ((_, PElem _) : tail) = Just tail
deleteArg _ = Nothing

justPElem (PElem a) = Just a
justPElem _ = Nothing

passB = BwdPass
	{ bp_lattice = argLattice
	, bp_transfer = mkBTransfer transferB'
	, bp_rewrite = pureBRewrite $ rewriteExitB rewriteB
	}

runB :: Pass ArgFact ArgFact
runB = runPass (analyzeAndRewriteBwd passB) const
