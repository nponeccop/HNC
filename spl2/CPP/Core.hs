module CPP.Core where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Debug.Trace

import HN.Intermediate

import CPP.Intermediate
import CPP.Visualise
import CPP.TypeProducer
import Utils

import SPL.Types

-- inherited attributes for Definition
data DefinitionInherited = DefinitionInherited {
	diLevel           :: Int
,	diSymTab          :: M.Map String CppAtomType
,	diType            :: Maybe T
,	diTraceP          :: Bool
,	diRootTypes       :: M.Map String T
,	diTyped           :: C
}

-- synthesized attributes for Definition
data DefinitionSynthesized = DefinitionSynthesized {
	dsCppDef :: CppDefinition
}

-- support for CTyped
getDefType (CTyped t _) = t

sem_Definition inh self @ (Definition name args val wh)
	= DefinitionSynthesized {
		dsCppDef = CppFunctionDef {
			functionLevel 			= diLevel inh
		,	functionTemplateArgs	= S.toList $ typePolyVars inhType
		,	functionIsStatic		= isFunctionStatic self
		,	functionReturnType 		= case cppDefType of CppTypeFunction returnType _ -> returnType ; _ -> cppDefType
		,	functionContext			= ctx
		,	functionName 			= name
		,	functionArgs 			= zipWith CppVarDecl (case cppDefType of CppTypeFunction _ argTypes -> argTypes ; _ -> []) args
		,	functionLocalVars 		= wsLocalVars semWhere
		,	functionRetExpr			= sem_Expression (symTabTranslator symTabWithoutArgsAndLocals) val
		}
	} where
		ctx = getContext (wsMethods semWhere) inh defType (wsTemplateArgs semWhere) self

		rt = diRootTypes inh
		defType = getDefType (diTyped inh)

		exprOutputTypes = case inhType of
			TUL (_: innerDefs) -> M.insert name (TUL innerDefs) rt
			_ -> M.delete name rt

		smartTrace x = if diTraceP inh then trace2 x else x

		cppDefType = cppUncurryType inhType args
		inhType = maybe defType id $ diType inh
		-- localsList : semWhere
		localsList = map (\(CppVar _ name _ ) -> name) (wsLocalVars semWhere)
		-- symTabWithStatics : semWhere
		symTabWithStatics = (wsLocalFunctionMap semWhere) `M.union` (diSymTab inh)

		-- symTabWithoutArgsAndLocals : self symTabWithStatics localsList
		symTabWithoutArgsAndLocals = symTabWithStatics `subtractKeysFromMap` args `subtractKeysFromMap` localsList

		semWhere = sem_Where (WhereInherited symTabT classPrefix isFunctionStatic exprOutputTypes inh { diLevel = diLevel inh + 1 }) wh where
			classPrefix = CppFqMethod $ (contextTypeName $ fromJust ctx) ++ (showTemplateArgs $ contextTemplateArgs $ fromJust ctx)
			-- symTabT : symTabWithStatics
			symTabT = symTabTranslator symTabWithStatics

		isFunctionStatic def  = S.null $ getDefinitionFreeVarsWithoutFqn def where
			getDefinitionFreeVarsWithoutFqn def = getDefinitionFreeVars def `subtractSet` M.keysSet (diSymTab inh)

data WhereSynthesized d e = WhereSynthesized {
	wsLocalVars :: [CppLocalVarDef]
,	wsLocalFunctionMap :: M.Map String CppAtomType
,	wsMethods :: d
,	wsTemplateArgs :: e
}

data WhereInherited a b c d e f = WhereInherited {
	wiSymTabT          :: a
,	wiClassPrefix      :: b
,	wiIsFunctionStatic :: c
,	wiTypes            :: f
,	wiDi               :: e
}

sem_Where inh self
	= WhereSynthesized {
		wsLocalVars = wsLocalVars
	,	wsLocalFunctionMap = M.fromList $ mapPrefix (wiClassPrefix inh) (wiIsFunctionStatic inh) ++ mapPrefix CppContextMethod (not . wiIsFunctionStatic inh)
	,	wsMethods = map (\x -> x { functionTemplateArgs = [] }) wsMethods'
	,	wsTemplateArgs = nub $ (concat $ (map functionTemplateArgs wsMethods') ++ varTemplateArgs)
	} where
		wsMethods' = getWhereMethods (wiDi inh) (wiTypes inh) self
		wsLocalVars' = getWhereVars (wiSymTabT inh) (wiTypes inh) self
		varTemplateArgs = map vdsTemplateArgs wsLocalVars'
		wsLocalVars = map vdsVarDef wsLocalVars'
		mapPrefix prefix fn = map (\def -> (defName def, prefix)) $ filter (\x -> isFunction x && (fn x)) self

isFunction (Definition _ args _ _) = not $ null args

symTabTranslator symTab f x = case M.lookup x symTab of
	Just (CppFqMethod prefix) -> prefix ++ "::" ++ x
	Just CppContextMethod -> if f then "impl." ++ x else "hn::bind(impl, &local::" ++ x ++ ")"
	Nothing -> x

getContext methods inh defType templateVars (Definition name args _ wh)
	= constructJust (null vars && null methods) $ CppContext (diLevel inh) contextTemplateVars (name ++ "_impl") vars methods where

	-- переменные контекста - это
	-- аргументы главной функции, свободные в where-функциях
	-- локальные переменные, свободные в where-функциях
	vars = (filter (\(CppVar _ name _ ) -> not $ S.member name lvn) $ (map vdsVarDef varSem))  ++ contextArgs where
		lvn = getWhereVarNames wh

	varSem = getWhereVars (symTabTranslator $ diSymTab inh) (diRootTypes inh) wh
	contextTemplateVars = nub ((templateVars ++ (concat $ (map vdsTemplateArgs varSem)) ++ (S.toList $ S.unions $ contextArgsTv)))

	(contextArgs, contextArgsTv) = unzip $ case defType of
		TT funList -> map (\(typ, x) -> (CppVar (cppType typ) x $ CppAtom x, typePolyVars typ)) $ filter (\(_, y) -> isArgContext y) $ zip (init funList) args
		_ -> []
	wfv = getSetOfListFreeVars (filter isFunction wh)
	isArgContext a = S.member a wfv


isVar (Definition _ args _ _) = null args
getFromWhere wh mf ff = map mf $ filter ff wh

getWhereVars fqn wiTypes def = getFromWhere def (sem_VarDefinition fqn wiTypes) isVar

getWhereMethods inh whereTypes wh = getFromWhere wh (\def -> dsCppDef $ sem_Definition (f def) def) (not . isVar)
	where
		f def = inh { diType = Just $ uncondLookup (defName def) whereTypes }

defName (Definition name _ _ _) = name

getWhereX wh f = S.fromList $ getFromWhere wh defName f

getWhereVarNames wh = getWhereX wh isVar
getWhereAtoms wh =  getWhereX wh (const True)

getXVars fn def = fn (getWhereFreeVars def) (getDefinitionFreeVars def) where
	-- переменные, свободные в данном where. Могут быть контекстными или внешними
	getWhereFreeVars (Definition _ _ _ wh) = getSetOfListFreeVars wh

getSetOfListFreeVars ww = S.unions $ map getDefinitionFreeVars ww

data VarDefinitionSynthesized a b = VarDefinitionSynthesized {
	vdsVarDef :: a
,	vdsTemplateArgs :: b
}

sem_VarDefinition fqn wiTypes (Definition name [] val _) =
	VarDefinitionSynthesized {
		vdsVarDef = CppVar (cppType inferredType) name $ sem_Expression fqn val
	,	vdsTemplateArgs = S.toList $ typePolyVars inferredType
	}
	 where
		inferredType = uncondLookup name wiTypes

sem_Expression fqn p = case p of
	Atom x -> CppAtom $ fqn False x
	Constant x -> CppLiteral x
	-- либо hn::bind(impl, &main_impl::a) - CppApplication CppAtom "hn::bind" [ CppAtom "impl", CppPtr (CppAtom a) ]
	-- либо impl.a - CppField impl a
	-- либо &main_impl::a - CppPtr (CppAtom a)
	Application x y -> CppApplication transformApplicand (map transformOperand y) where
		transformApplicand = case x of
			Atom a -> CppAtom $ fqn True a
			_ -> sem_Expression fqn x
		transformOperand (Atom a) = CppAtom $ if elem ':' aaa && (not $ isPrefixOf "hn::" aaa) then '&' : aaa else aaa where
				aaa = fqn False a
		transformOperand x = sem_Expression fqn x

getExpressionAtoms (Atom x) = S.singleton x
getExpressionAtoms (Application a b) = S.unions $ map getExpressionAtoms (a : b)
getExpressionAtoms _ = S.empty

getDefinitionFreeVars (Definition _ args val wh)
	= (S.union (getExpressionAtoms val) (getSetOfListFreeVars wh)) `subtractSet` (S.fromList args) `subtractSet` (getWhereAtoms wh)
