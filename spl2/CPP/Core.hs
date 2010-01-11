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
import qualified Bar as AG

-- inherited attributes for Definition
data DefinitionInherited = DefinitionInherited {
	diLevel           :: Int
,	diSymTab          :: M.Map String CppAtomType
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
		,	functionName 			= AG.name_Syn_Definition semDef
		,	functionArgs 			= zipWith CppVarDecl (case cppDefType of CppTypeFunction _ argTypes -> argTypes ; _ -> []) args
		,	functionLocalVars 		= wsLocalVars semWhere
		,	functionRetExpr			= AG.retExpr_Syn_Definition semDef
		}
	} where
		semDef =  AG.wrap_Definition (AG.sem_Definition self)
			(AG.Inh_Definition {
			   AG.fqn_Inh_Definition = symTabTranslator symTabWithoutArgsAndLocals
			})

		ctx = getContext (wsMethods semWhere) inh defType (wsTemplateArgs semWhere) self

		rt = diRootTypes inh
		defType = getDefType (diTyped inh)

{-		exprOutputTypes = case inhType of
			TUL (_: innerDefs) -> M.insert name (TUL innerDefs) rt
			_ -> M.delete name rt
-}
		exprOutputTypes = whereList $ traceU (show $ wh) $ diTyped inh

		smartTrace x = if diTraceP inh then trace2 x else x

		cppDefType = cppUncurryType inhType args
		inhType = defType
		-- localsList : semWhere
		localsList = map (\(CppVar _ name _ ) -> name) (wsLocalVars semWhere)
		-- symTabWithStatics : semWhere
		symTabWithStatics = wsLocalFunctionMap semWhere `M.union` diSymTab inh

		-- symTabWithoutArgsAndLocals : self symTabWithStatics localsList
		symTabWithoutArgsAndLocals = symTabWithStatics `subtractKeysFromMap` args `subtractKeysFromMap` localsList

		semWhere = sem_Where (WhereInherited symTabT classPrefix isFunctionStatic (traceU ("exprOutputTypes = " ++ show exprOutputTypes) exprOutputTypes) inh { diLevel = diLevel inh + 1 }) wh where
			classPrefix = CppFqMethod $ contextTypeName (fromJust ctx) ++ showTemplateArgs (contextTemplateArgs $ fromJust ctx)
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

whereList tt = case tt of
	CTyped _ (CL (CTyped (TT (a : _)) (CL xx (S (b : _)))) _) -> M.insert b a (whereList xx)
	CTyped _ (CL (CTyped (a) (CL (CTyped _ (CL xx (S (b : _)))) y)) _) -> M.insert b a (whereList xx)
	_ -> M.empty
--	CTyped _ (CL (CTyped a (CL (CVal b) _)) _) -> M.fromList [(b, a)]

traceU x y = y

sem_Where inh self
	= WhereSynthesized {
		wsLocalVars = wsLocalVars
	,	wsLocalFunctionMap = M.fromList $ mapPrefix (wiClassPrefix inh) (wiIsFunctionStatic inh) ++ mapPrefix CppContextMethod (not . wiIsFunctionStatic inh)
	,	wsMethods = map (\x -> x { functionTemplateArgs = [] }) wsMethods'
	,	wsTemplateArgs = nub $ concat $ map functionTemplateArgs wsMethods' ++ varTemplateArgs
	} where
		wsMethods' = getWhereMethods (wiDi inh) (wiTypes inh) (diTyped $ wiDi inh) self
		wsLocalVars' = getWhereVars (wiSymTabT inh) (wiTypes inh) self
		varTemplateArgs = map vdsTemplateArgs wsLocalVars'
		wsLocalVars = map vdsVarDef wsLocalVars'
		mapPrefix prefix fn = map (\def -> (defName def, prefix)) $ filter (\x -> isFunction x && fn x) self

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
	vars = filter (\(CppVar _ name _ ) -> not $ S.member name lvn) (map vdsVarDef varSem)  ++ contextArgs where
		lvn = getWhereVarNames wh

	varSem = getWhereVars (symTabTranslator $ diSymTab inh) (diRootTypes inh) wh
	contextTemplateVars = nub ((templateVars ++ concat (map vdsTemplateArgs varSem)) ++ S.toList (S.unions contextArgsTv))

	(contextArgs, contextArgsTv) = unzip $ case defType of
		TT funList -> map (\(typ, x) -> (CppVar (cppType typ) x $ CppAtom x, typePolyVars typ)) $ filter (\(_, y) -> isArgContext y) $ zip (init funList) args
		_ -> []
	wfv = getSetOfListFreeVars (filter isFunction wh)
	isArgContext a = S.member a wfv


isVar (Definition _ args _ _) = null args
getFromWhere wh mf ff = map mf $ filter ff wh

getWhereVars fqn wiTypes def = getFromWhere def sem_VarDefinition isVar where
	sem_VarDefinition (Definition name [] val _) =
		VarDefinitionSynthesized {
			vdsVarDef = CppVar (cppType inferredType) name $ AG.sem_Expression2 fqn val
		,	vdsTemplateArgs = S.toList $ typePolyVars inferredType
		} where
			inferredType = uncondLookup name wiTypes

getWhereMethods inh whereTypes whereTyped wh = getFromWhere wh (\def -> dsCppDef $ sem_Definition (f def) def) (not . isVar)
	where
--		f def = inh { diType = Just $ traceU ("getWhereMethods: whereTypes = " ++ show whereTypes) $ uncondLookup (defName def) whereTypes, diTyped = trace2 $ whereTyped }
		f def = inh { diTyped = getWhereTyped whereTyped }

		getWhereTyped (CTyped _ (CL x (K (y : _)))) = y
		getWhereTyped x = error "Not supported at getWhereMethods"

defName (Definition name _ _ _) = name

getWhereX wh = S.fromList . getFromWhere wh defName

getWhereVarNames wh = getWhereX wh isVar
getWhereAtoms wh =  getWhereX wh (const True)

getSetOfListFreeVars = S.unions . map getDefinitionFreeVars

data VarDefinitionSynthesized a b = VarDefinitionSynthesized {
	vdsVarDef :: a
,	vdsTemplateArgs :: b
}


getDefinitionFreeVars (Definition _ args val wh)
	= S.union (AG.getExpressionAtoms val) (getSetOfListFreeVars wh) `subtractSet` S.fromList args `subtractSet` getWhereAtoms wh
