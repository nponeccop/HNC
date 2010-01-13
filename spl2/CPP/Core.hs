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

sem_Definition inh self @ (Definition name args val wh)
	= DefinitionSynthesized {
		dsCppDef = CppFunctionDef {
			functionLevel 			= diLevel inh
		,	functionTemplateArgs	= S.toList $ typePolyVars defType
		,	functionIsStatic		= isFunctionStatic self
		,	functionReturnType 		= case cppDefType of CppTypeFunction returnType _ -> returnType ; _ -> cppDefType
		,	functionContext			= ctx
		,	functionName 			= AG.name_Syn_Definition semDef
		,	functionArgs 			= zipWith CppVarDecl (case cppDefType of CppTypeFunction _ argTypes -> argTypes ; _ -> []) args
		,	functionLocalVars 		= wsVars semWhere
		,	functionRetExpr			= AG.retExpr_Syn_Definition semDef
		}
	} where
		semDef =  AG.wrap_Definition (AG.sem_Definition self)
			AG.Inh_Definition {
			   AG.fqn_Inh_Definition = symTabTranslator symTabWithoutArgsAndLocals
			}

		ctx = sem_Context self ContextInherited {
				ciSemWhere = semWhere
			,   ciDefType = defType
			, 	ciDi = inh
		}

		semWhere = sem_Where wh WhereInherited {
					wiSymTabT          = symTabTranslator symTabWithStatics
				,	wiClassPrefix      = CppFqMethod $ contextTypeName (fromJust ctx) ++ showTemplateArgs (contextTemplateArgs $ fromJust ctx)
				,	wiIsFunctionStatic = isFunctionStatic
				,	wiTypes            = whereList $ traceU ("sem_Definition.diTyped inh = " ++ show (diTyped inh)) $ diTyped inh
				,	wiDi               = inh { diLevel = diLevel inh + 1 }
				}

		defType = getDefType (diTyped inh)

{-		exprOutputTypes = case inhType of
			TUL (_: innerDefs) -> M.insert name (TUL innerDefs) rt
			_ -> M.delete name rt
-}
		smartTrace x = if diTraceP inh then trace2 x else x

		cppDefType = cppUncurryType defType args

		-- localsList : semWhere
		localsList = map (\(CppVar _ name _ ) -> name) (wsVars semWhere)
		-- symTabWithStatics : semWhere
		symTabWithStatics = wsLocalFunctionMap semWhere `M.union` diSymTab inh

		-- symTabWithoutArgsAndLocals : self symTabWithStatics localsList
		symTabWithoutArgsAndLocals = symTabWithStatics `subtractKeysFromMap` args `subtractKeysFromMap` localsList

		isFunctionStatic def  = S.null $ getDefinitionFreeVars def `subtractSet` M.keysSet (diSymTab inh)

data WhereSynthesized d e = WhereSynthesized {
	wsVars :: [CppLocalVarDef]
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

sem_Where self inh
	= WhereSynthesized {
		wsVars = map vdsVarDef wsVars1
	,	wsLocalFunctionMap = M.fromList $ mapPrefix (wiClassPrefix inh) (wiIsFunctionStatic inh) ++ mapPrefix CppContextMethod (not . wiIsFunctionStatic inh)
	,	wsMethods = map (\x -> x { functionTemplateArgs = [] }) wsMethods1
	,	wsTemplateArgs = nub $ concat $ map functionTemplateArgs wsMethods1 ++ map vdsTemplateArgs wsVars1
	} where
		wsMethods1   = sem_WhereMethods (wiDi inh)      (diTyped $ wiDi inh) self
		wsVars1      = sem_WhereVars    (wiSymTabT inh) (wiTypes inh)        self
		mapPrefix prefix fn = map (\def -> (defName def, prefix)) $ filter (\x -> isFunction x && fn x) self

data VarDefinitionSynthesized a b = VarDefinitionSynthesized {
	vdsVarDef :: a
,	vdsTemplateArgs :: b
}

sem_WhereVars fqn wiTypes wh = getFromWhere wh sem_VarDefinition isVar where
	sem_VarDefinition (Definition name [] val _) =
		VarDefinitionSynthesized {
			vdsVarDef = CppVar (cppType inferredType) name $ AG.sem_Expression2 fqn val
		,	vdsTemplateArgs = S.toList $ typePolyVars inferredType
		} where
			inferredType = traceU ("sem_VarDefinition: wiTypes = " ++ show wiTypes) $ uncondLookup name wiTypes

sem_WhereMethods inh whereTyped wh = getFromWhere wh sem_MethodDefinition (not . isVar) where
	sem_MethodDefinition = dsCppDef . sem_Definition newInh
	newInh = inh {
		diTyped =  case whereTyped of
			CTyped _ (CL _ (K (y @ (CTyped _ _) : _))) -> y
			CTyped _ (CL x @ (CTyped _ _) (S _)) -> x
			x -> error $ "sem_WhereMethods: " ++ show x
	}

data ContextInherited a b c = ContextInherited {
	ciSemWhere :: a
,	ciDi :: b
,	ciDefType :: c
}

sem_Context (Definition name args _ wh) inh
	= constructJust (null vars && null methods) CppContext {
		   contextLevel        = diLevel $ ciDi inh
		,  contextTemplateArgs = nub ((templateVars ++ concat (map vdsTemplateArgs varSem)) ++ S.toList (S.unions contextArgsTv))
		,  contextTypeName	   = name ++ "_impl"
		-- переменные контекста - это
		-- аргументы главной функции, свободные в where-функциях
		-- локальные переменные, свободные в where-функциях
 		,  contextVars 	  	   = vars
 		,  contextMethods 	   = methods
	} where

	methods = wsMethods $ ciSemWhere inh
 	templateVars = wsTemplateArgs $ ciSemWhere inh

	vars = filter (\(CppVar _ name _ ) -> not $ S.member name lvn) (map vdsVarDef varSem)  ++ contextArgs where
		lvn = getWhereVarNames wh

	varSem = sem_WhereVars (symTabTranslator $ diSymTab $ ciDi inh) (traceU ("getContext.varSem" ++ show (diRootTypes $ ciDi inh)) $ diRootTypes $ ciDi inh) wh

	(contextArgs, contextArgsTv) = unzip $ case ciDefType inh of
		TT funList -> map (\(typ, x) -> (CppVar (cppType typ) x $ CppAtom x, typePolyVars typ)) $ filter (\(_, y) -> isArgContext y) $ zip (init funList) args
		_ -> []
	wfv = getSetOfListFreeVars (filter isFunction wh)
	isArgContext a = S.member a wfv

-- support for CTyped
getDefType (CTyped t _) = t
getDefType (t) = error $ "getDefType: " ++ show t

typeMap vars = M.fromList . zip vars . map (\(CTyped t _) -> t)

whereList tt = case tt of
	CTyped _ (CL (CL (CTyped _ (CL (CL _ (S vars2)) (K types2))) (S vars)) (K types)) -> typeMap (vars ++ vars2) (types ++ types2)
	CTyped _ (CL (CL _ (S vars)) (K types)) -> typeMap vars types
	CTyped _ (CL (CTyped _ (CL (CL _ (S vars)) (K types))) _) -> typeMap vars types
	CTyped _ (CL (CL (CL (CTyped _ (CL (CL (CTyped _ (CL (CL _ (S vars2)) (K types2))) (S vars)) (K types))) _) _) _) -> typeMap (vars ++ vars2) (types ++ types2)
	_ -> error $ "non-exhaustive patterns in whereList: " ++ show tt

traceU x y = y

isFunction (Definition _ args _ _) = not $ null args

symTabTranslator symTab f x = case M.lookup x symTab of
	Just (CppFqMethod prefix) -> prefix ++ "::" ++ x
	Just CppContextMethod -> if f then "impl." ++ x else "hn::bind(impl, &local::" ++ x ++ ")"
	Nothing -> x

isVar (Definition _ args _ _) = null args
getFromWhere wh mf ff = map mf $ filter ff wh

defName (Definition name _ _ _) = name

getWhereX wh = S.fromList . getFromWhere wh defName

getWhereVarNames wh = getWhereX wh isVar
getWhereAtoms wh =  getWhereX wh (const True)

getSetOfListFreeVars = S.unions . map getDefinitionFreeVars

getDefinitionFreeVars (Definition _ args val wh)
	= S.union (AG.getExpressionAtoms val) (getSetOfListFreeVars wh) `subtractSet` S.fromList args `subtractSet` getWhereAtoms wh
