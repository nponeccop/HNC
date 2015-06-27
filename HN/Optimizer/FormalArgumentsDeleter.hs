{-# LANGUAGE GADTs, TypeFamilies, NoMonomorphismRestriction, FlexibleContexts #-}
module HN.Optimizer.FormalArgumentsDeleter (runB) where

import Compiler.Hoopl hiding ((<*>))
import HN.Intermediate
import HN.Optimizer.Node
--import HN.Optimizer.Pass
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.ArgumentValues (ArgFact)
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

{-
listLattice = addPoints' "ListFact" $ \_ (OldFact _) (NewFact new) -> error "Join" (SomeChange, PElem new)

-- BACKWARD pass

transferBL :: BwdTransfer Node ListFact
transferBL = mkBTransfer bt where
  bt :: Node e x -> Fact x ListFact -> ListFact
  bt (Entry _)  f = f
  bt (Exit dn) _ = PElem dn

-}

-- Rewriting: inline definition - rewrite Exit nodes ("call sites") to
-- remove references to the definition being inlined
--

type ListFact = ArgFact

rewriteBL :: FuelMonad m => BwdRewrite m Node ListFact
rewriteBL = mkBRewrite (\a b -> return $ cp a b) where
	cp :: Node e x -> Fact x ListFact -> Maybe (Graph Node e x)
	cp (Entry _) _ = Nothing
	cp (Exit xll) f = case xll of
		LibNode -> Nothing
		ArgNode -> Nothing
		LetNode l expr -> (mkLast . Exit . LetNode l) <$> process (rewriteExpression f) expr

rewriteExpression f (Application aa @ (Atom a) b) = Application aa <$> rewriteArguments b (justPElem =<< lookupFact a f)
rewriteExpression _ _ = Nothing

rewriteArguments b f = map fst <$> (process deleteArg =<< zip b <$> f)

deleteArg :: Rewrite [(ExpressionFix, WithTopAndBot ExpressionFix)]
deleteArg ((_, PElem _) : tail) = Just tail
deleteArg _ = Nothing

justPElem (PElem a) = Just a
justPElem _ = Nothing

runB = rewriteBL
{-
passBL = BwdPass
	{ bp_lattice = listLattice
	, bp_transfer = transferBL
	, bp_rewrite = rewriteBL
	}

runB = runPass (analyzeAndRewriteBwd passBL) $ const . mapMap int2list where
	int2list 1 = Bot
	int2list _ = Top
-}
