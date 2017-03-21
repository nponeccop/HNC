{-# LANGUAGE GADTs, TypeFamilies, NoMonomorphismRestriction, LambdaCase #-}
module HN.Optimizer.Inliner2 (runB) where

import Compiler.Hoopl
import HN.Intermediate
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.Utils

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


listLattice = addPoints' "ListFact" $ \_ (OldFact _) (NewFact new) -> error "Join" (SomeChange, PElem new)

-- BACKWARD pass

transferB :: Label -> DefinitionNode -> FactBase ListFact -> ListFact
transferB ll dn @ (LetNode _ (Application (Atom l) _)) _ | l == ll = Top
transferB ll dn _ = PElem dn

-- Rewriting: inline definition - rewrite Exit nodes ("call sites") to
-- remove references to the definition being inlined

rewriteB :: DefinitionNode -> FactBase ListFact -> Maybe DefinitionNode
rewriteB xll f = case xll of
	LibNode -> Nothing
	ArgNode -> Nothing
	LetNode l expr ->  LetNode l <$> rewriteExpression f expr
		
passBL whileLabel = BwdPass
	{ bp_lattice = listLattice
	, bp_transfer = mkBTransfer $ transferExitB $ transferB whileLabel
	, bp_rewrite = pureBRewrite $ rewriteExitB rewriteB
	}

runB :: Label -> Pass Int ListFact
runB whileLabel = runPass (analyzeAndRewriteBwd $ passBL whileLabel) $ const . mapMap int2list where
	int2list 1 = Bot
	int2list _ = Top

type ListFact = WithTopAndBot DefinitionNode

rewriteExpression :: FactBase ListFact -> Rewrite ExpressionFix
rewriteExpression f = rewrite WithChildren $ \case
		Constant _ -> Nothing
		Application _ _ -> Nothing
		Atom a -> case lookupFact a f of
			Nothing -> error "Lone.uncondLookupFact.Nothing"
			Just Bot -> error "Lone.rewriteExitL.Bot"
			Just (PElem (LetNode [] body)) -> Just body
			_ -> Nothing

