module CPP.Core where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe

import HN.Intermediate

import CPP.Intermediate
import CPP.Visualise
import CPP.TypeProducer
import Utils
import Debug.Trace

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

typeTemplateArgs = S.toList . typePolyVars

sem_Definition inh self @ (Definition name args val wh)
	= DefinitionSynthesized {
		dsCppDef = (AG.cppDefinition_Syn_Definition semDef) {
			functionTemplateArgs	= typeTemplateArgs defType
		,	functionReturnType 		= case cppDefType of CppTypeFunction returnType _ -> returnType ; _ -> cppDefType
		,	functionContext			= ctx
		,	functionArgs 			= zipWith CppVarDecl (case cppDefType of CppTypeFunction _ argTypes -> argTypes ; _ -> []) args
		,	functionLocalVars 		= wsVars semWhere
		}
	} where

		agInh = AG.Inh_Definition {
				AG.level_Inh_Definition = diLevel inh
			, 	AG.typed_Inh_Definition = diTyped inh
			,	AG.fqn_Inh_Definition = symTabTranslator $ symTabWithStatics `subtractKeysFromMap` args `subtractKeysFromMap` map (\(CppVar _ name _ ) -> name) (wsVars semWhere)
			,   AG.symTab_Inh_Definition = diSymTab inh
			}
		semDef = AG.wrap_Definition (AG.sem_Definition self) agInh

		ctx0 = sem_Context self (null $ wsMethods semWhere) ContextInherited {
				ciSemWhere = semWhere
			,   ciDefType = defType
			, 	ciDi = inh
			,   ciLevel = AG.level_Inh_Definition agInh
		}

		ctx = fmap (\ctt -> ctt { contextMethods = wsMethods semWhere }) ctx0

		semWhere = sem_Where wh WhereInherited {
					wiSymTabT          = symTabTranslator symTabWithStatics
				,	wiClassPrefix      = CppFqMethod $ contextTypeName (fromJust ctx) ++ showTemplateArgs (contextTemplateArgs $ fromJust ctx)
				,	wiIsFunctionStatic = isFunctionStatic
				,	wiTypes            = AG.deconstructTyped $ diTyped inh
				,	wiDi               = inh { diLevel = AG.level_Inh_Definition agInh + 1 }
				}

		defType = AG.defType $ diTyped inh

		cppDefType = cppUncurryType defType args

		symTabWithStatics = wsLocalFunctionMap semWhere `M.union` diSymTab inh
		isFunctionStatic def  = S.null $ (AG.freeVars_Syn_Definition semDef) `subtractSet` M.keysSet (diSymTab inh)


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

sem_WhereVars fqn wiTypes wh = getFromWhere wh sem_VarDefinition (not . isFunction) where
	sem_VarDefinition (Definition name [] val _) =
		VarDefinitionSynthesized {
			vdsVarDef = CppVar (cppType inferredType) name $ AG.sem_Expression2 fqn val
		,	vdsTemplateArgs = typeTemplateArgs inferredType
		} where
			inferredType = traceU ("sem_VarDefinition: wiTypes = " ++ show wiTypes) $ uncondLookup name wiTypes

sem_WhereMethods inh whereTyped wh = getFromWhere wh sem_MethodDefinition isFunction where
	sem_MethodDefinition = dsCppDef . sem_Definition newInh
	newInh = inh {
		diTyped =  case whereTyped of
			CTyped _ (CL _ (K (y @ (CTyped _ _) : _))) -> y
			CTyped _ (CL x @ (CTyped _ _) (S _)) -> x
			x -> error $ "sem_WhereMethods: " ++ show x
	}

data ContextInherited a b c d = ContextInherited {
	ciSemWhere :: a
,	ciDi :: b
,	ciDefType :: c
,   ciLevel :: d
}

sem_Context (Definition name args _ wh) noMethods inh
	= constructJust (null vars && noMethods) CppContext {
		   contextLevel        = ciLevel inh
		,  contextTemplateArgs = S.toList $ S.unions $ S.fromList templateVars : S.fromList (concat $ map vdsTemplateArgs varSem) : contextArgsTv
		,  contextTypeName	   = name ++ "_impl"
		-- переменные контекста - это
		-- аргументы главной функции, свободные в where-функциях
		-- локальные переменные, свободные в where-функциях
 		,  contextVars 	  	   = vars
	} where

 	templateVars = wsTemplateArgs $ ciSemWhere inh

	vars = filter (\(CppVar _ name _ ) -> not $ S.member name lvn) (map vdsVarDef varSem)  ++ contextArgs where
		lvn = S.fromList $ getFromWhere wh defName $ not . isFunction

	varSem = sem_WhereVars (symTabTranslator $ diSymTab $ ciDi inh) (diRootTypes $ ciDi inh) wh

	(contextArgs, contextArgsTv) = case ciDefType inh of
		TT funList -> unzip $ map (\(typ, x) -> (CppVar (cppType typ) x $ CppAtom x, typePolyVars typ)) $ filter (\(_, y) -> isArgContext y) $ zip (init funList) args
		_ -> ([], []) where

	isArgContext a = S.member a $ getSetOfListFreeVars (filter isFunction wh) where
		getSetOfListFreeVars = S.unions . map getDefinitionFreeVars where
			getDefinitionFreeVars (Definition _ args val wh)
				= AG.getExpressionAtoms val `S.union` getSetOfListFreeVars wh `subtractSet` S.fromList args `subtractSet` S.fromList (map defName wh)

traceU x y = y

isFunction (Definition _ args _ _) = not $ null args

symTabTranslator symTab f x = case M.lookup x symTab of
	Just (CppFqMethod prefix) -> prefix ++ "::" ++ x
	Just CppContextMethod -> if f then "impl." ++ x else "hn::bind(impl, &local::" ++ x ++ ")"
	Nothing -> x

getFromWhere wh mf ff = map mf $ filter ff wh

defName (Definition name _ _ _) = name
