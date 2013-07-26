{-

Идея - использовать Hoopl как фреймворк произвольных оптимизаторов графов,
а не только "императивной лапши с Goto", как описано в статье

-}
{-# LANGUAGE GADTs, TypeFamilies, NoMonomorphismRestriction #-}
module HN.Optimizer.Inliner2 (runB) where

import Compiler.Hoopl
import HN.Intermediate
import HN.Optimizer.Node
import HN.Optimizer.Pass
import Utils
import HN.Optimizer.Visualise ()
{-
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
-}

--data Node0 e x where
--	Node0 :: Label -> [Label] -> Node0 C С

-- для блоков из одного СС-узла нет вспомогательных функций

type ListFact = WithTopAndBot DefinitionNode

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
rewriteBL = mkBRewrite cp where
	cp ::  FuelMonad m => Node e x -> Fact x ListFact -> m (Maybe (Graph Node e x))
	cp (Entry _) _ = return Nothing
	cp (Exit xll) f = return $ rewriteExitL xll f

rewriteExitL dn f = case dn of
	LibNode -> Nothing
	ArgNode -> Nothing
	LetNode l expr -> fmap (nodeToG . Exit . LetNode l) $ rewriteExpression expr f

rewriteApplication (Atom a) b f = case lookupFact a f of
	Nothing -> error "rapp.Atom.Nothing"
	Just x -> xtrace ("rewriteAtomApplication of " ++ show a ++ " to " ++ show b) $ rewriteAtomApplication $ xtrace ("rewriteAtomApplication fact " ++ show a) x where
		rewriteAtomApplication :: ListFact -> Maybe (Expression Label)
		rewriteAtomApplication aDef = case aDef of
			Top -> fmap (Application $ Atom a) $ rewriteArgs (xtrace "Top.b" b) f
			Bot -> error "rapp.bot"
			PElem x -> case x of
				LetNode [] expr -> Just $ Application expr $ case rewriteArgs b f of
					Nothing -> b
					Just b' -> b'
				LetNode args expr -> xtrace ("rewriteAtomApplication rewritten LetNode " ++ show a) $ inlineApplication expr args b f
				LibNode -> fmap (Application $ Atom a) $ rewriteArgs (xtrace "LibNode.b" b) f
				ArgNode -> fmap (Application $ Atom a) $ rewriteArgs (xtrace "ArgNode.b" b) f	

rewriteApplication (Application (Atom a) b) c f = case lookupFact a f of
	Nothing -> error "rewriteApplication.double.Nothing"
	Just x -> case x of
		Top -> error "rewriteApplication.double.Top"
		Bot -> error "rewriteApplication.double.Bot"
		PElem x -> case x of
 			LetNode [] _ -> error "rewriteApplication.double.LetNode.var"
 			LetNode outerParams body -> case body of
 				Atom aOuterBody -> case lookupFact aOuterBody f of
					Just (PElem (LetNode innerParams innerBody)) -> case inlineApplication innerBody innerParams c f of
						Just (Application aa bb) -> case inlineApplication aa outerParams b f of
							Just x -> Just (Application x bb)
							Nothing -> Just (Application aa bb)
						Just _ -> error "rewriteApplication.double.LetNode.fn.Just.noApp"
						Nothing -> Nothing						
					_ -> error "rewriteApplication.double.LetNode.fn.atombody.cannotInline"
 				_ -> error "rewriteApplication.double.LetNode.fn"
			LibNode -> error "rewriteApplication.double.LibNode"
			ArgNode -> Nothing -- error "rewriteApplication.double.ArgNode"

rewriteApplication a b f = case rewriteExpression a f of
	Nothing -> case rewriteArgs b f of
		Nothing -> ztrace ("rewriteArgsNothing " ++ show b) Nothing
		Just b' -> Just $ (Application a) $ ztrace ("rewriteArgs " ++ show b) b' 
	Just _ -> error "rapp.Just" 

inlineApplication inlinedBody formalArgs actualArgs f = let f' = flip mapUnion f $ mapFromList $ zip (xtrace "formalArgs" formalArgs) $ map (PElem . LetNode []) actualArgs  
	in case xtrace ("inlineApp.inlineBody of " ++ show inlinedBody) $ rewriteExpression inlinedBody f' of
		Nothing -> Just $ xtrace "inlineApp.inlinedBody" inlinedBody
		Just x -> Just $ xtrace "inlineApp.rewrittenBody" x

rewriteArgs [] _ = Nothing
rewriteArgs (a : at) f = xtrace ("rewriteArgs " ++ show (a : at) ++ " is ") $ case (rewriteArgs at f, rewriteExpression a f) of
	(Nothing , Nothing) -> Nothing
	(Nothing , Just a') -> Just $ a' : at
	(Just at', Nothing) -> Just $ a : at'
	(Just at', Just a') -> Just $ a' : at'

rewriteExpression expr f =  case expr of
	Constant _ -> Nothing
	Atom a -> xtrace ("rewriteExpression Atom " ++ show a) $ case lookupFact a f of
		Nothing -> error "rewriteExpression.Nothing"
		Just x -> processAtom x
	Application a b -> xtrace ("rewriteExpression.rewriteApplication of " ++ show a ++ " to " ++ show b) $ rewriteApplication a b f

processAtom :: ListFact -> Maybe (Expression Label)
processAtom x = case x of
 	Top -> Nothing
 	Bot -> error "rewriteExitL.Bot"
 	PElem e -> case e of
		ArgNode -> error "processFact.ArgNode"
		LetNode [] e -> Just e
		LetNode _ _ -> Nothing
		LibNode -> Nothing

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


{-
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
