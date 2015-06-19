{-# LANGUAGE GADTs, TypeFamilies, NoMonomorphismRestriction #-}
module HN.Optimizer.Inliner2 (runB) where

import Compiler.Hoopl
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.Rewriting

{-

Идея - использовать Hoopl как фреймворк произвольных оптимизаторов графов,
а не только "императивной лапши с Goto", как описано в статье

Инлайнинг:

Вход:

foo x = {
	bar x = incr x
	bar x
}

Факты:

bar -> 1

Выход (без учета других оптимизаций):

foo x = {
	bar x = incr x
	incr x
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
-}


listLattice = addPoints' "ListFact" $ \_ (OldFact _) (NewFact new) -> error "Join" (SomeChange, PElem new)

-- BACKWARD pass

transferBL :: BwdTransfer Node ListFact
transferBL = mkBTransfer bt where
  bt :: Node e x -> Fact x ListFact -> ListFact
  bt (Entry _)  f = f
  bt (Exit dn) _ = PElem dn

-- Rewriting: inline definition - rewrite Exit nodes ("call sites") to
-- remove references to the definition being inlined

rewriteBL :: FuelMonad m => BwdRewrite m Node ListFact
rewriteBL = mkBRewrite (\a b -> return $ cp a b) where
	cp :: Node e x -> Fact x ListFact -> Maybe (Graph Node e x)
	cp (Entry _) _ = Nothing
	cp (Exit xll) f = case xll of
		LibNode -> Nothing
		ArgNode -> Nothing
		LetNode l expr -> (nodeToG . Exit . LetNode l) <$> rewriteExpression f expr
		
passBL = BwdPass
	{ bp_lattice = listLattice
	, bp_transfer = transferBL
	, bp_rewrite = rewriteBL
	}

runB = runPass (analyzeAndRewriteBwd passBL) $ const . mapMap int2list where
	int2list 1 = Bot
	int2list _ = Top

-- Utilities
--
--
nodeToG :: Node e x -> Graph Node e x
nodeToG (Entry el) = mkFirst (Entry el)
nodeToG (Exit xll) = mkLast (Exit xll)

