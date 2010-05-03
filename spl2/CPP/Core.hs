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
,   diInferredType    :: T
}

-- synthesized attributes for Definition
data DefinitionSynthesized = DefinitionSynthesized {
	dsCppDef :: CppDefinition
}


sem_Definition inh self @ (Definition name args val wh)
	= DefinitionSynthesized {
		dsCppDef = (AG.cppDefinition_Syn_Definition semDef) {
			functionContext			=  fmap (\ctt -> ctt { contextMethods = wsMethods semWhere }) ctx
		}
	} where

		agInh = AG.Inh_Definition {
				AG.level_Inh_Definition = diLevel inh
			, 	AG.typed_Inh_Definition = diTyped inh
			,	AG.fqn_Inh_Definition = symTabTranslator $ symTabWithStatics `subtractKeysFromMap` (args ++ map (\(CppVar _ name _ ) -> name) flv)
			,   AG.symTab_Inh_Definition = diSymTab inh
			,   AG.inferredType_Inh_Definition = diInferredType inh
			}
		semDef = AG.wrap_Definition (AG.sem_Definition self) agInh

		flv = functionLocalVars $ AG.cppDefinition_Syn_Definition semDef

		agContext = fromJust $ functionContext $ AG.cppDefinition_Syn_Definition semDef

		ctx = sem_Context self agContext (null $ wsMethods semWhere) ContextInherited {
				ciSemWhere = semWhere
			,   ciDefType = AG.inferredType_Inh_Definition agInh
			, 	ciDi = inh
		}

		semWhere = sem_Where wh WhereInherited {
					wiSymTabT          = symTabTranslator symTabWithStatics
				,	wiTypes            = AG.deconstructTyped $ diTyped inh
				,	wiDi               = inh { diLevel = AG.level_Inh_Definition agInh + 1 }
				}

		symTabWithStatics = lfm (wsMapPrefix semWhere) `M.union` diSymTab inh

		lfm mapPrefix = M.fromList $ mapPrefix classPrefix isFunctionStatic ++ mapPrefix CppContextMethod (not . isFunctionStatic) where
			classPrefix = CppFqMethod $ contextTypeName (fromJust ctx) ++ showTemplateArgs (contextTemplateArgs $ fromJust ctx)

		isFunctionStatic def  = S.null $ (AG.freeVars_Syn_Definition xsemDef) `subtractSet` M.keysSet (diSymTab inh) where
			xsemDef = AG.wrap_Definition (AG.sem_Definition def) agInh

data WhereSynthesized d e f = WhereSynthesized {
	wsMethods :: d
,	wsTemplateArgs :: e
,   wsMapPrefix :: f
}

data WhereInherited a d e = WhereInherited {
	wiSymTabT          :: a
,	wiTypes            :: d
,	wiDi               :: e
}

sem_Where self inh
	= WhereSynthesized {
		wsMethods = map (\x -> x { functionTemplateArgs = [] }) wsMethods1
	,	wsTemplateArgs = nub $ concat $ map functionTemplateArgs wsMethods1 ++ wsVars1
	,   wsMapPrefix = mapPrefix
	} where
		wsMethods1   = sem_WhereMethods (wiDi inh)      (diTyped $ wiDi inh) self
		wsVars1      = sem_WhereVars    (wiTypes inh)                        self
		mapPrefix prefix fn = map (\def -> (defName def, prefix)) $ filter (\x -> isFunction x && fn x) self

sem_WhereVars wiTypes wh = getFromWhere wh sem_VarDefinition (not . isFunction) where
	sem_VarDefinition (Definition name [] val _) = AG.typeTemplateArgs $ uncondLookup name wiTypes


sem_WhereMethods inh whereTyped wh = getFromWhere wh sem_MethodDefinition isFunction where
	sem_MethodDefinition = dsCppDef . sem_Definition newInh
	newInh = inh {
		diTyped =  case whereTyped of
			CTyped _ (CL _ (K (y @ (CTyped _ _) : _))) -> y
			CTyped _ (CL x @ (CTyped _ _) (S _)) -> x
			x -> const x $ error $ "sem_WhereMethods: " ++ show x
	}

data ContextInherited a b c = ContextInherited {
	ciSemWhere :: a
,	ciDi :: b
,	ciDefType :: c
}

sem_Context (Definition name args _ wh) agContext noMethods inh
	= constructJust (null vars && noMethods) agContext {
		   contextTemplateArgs = S.toList $ S.unions $ S.fromList templateVars : S.fromList (concat $ varSem) : contextArgsTv
		-- переменные контекста - это
		-- аргументы главной функции, свободные в where-функциях
		-- локальные переменные, свободные в where-функциях (пока не поддерживается!)
 		,  contextVars 	  	   = vars
	} where

 	templateVars = wsTemplateArgs $ ciSemWhere inh

	vars = contextArgs

	varSem = sem_WhereVars (diRootTypes $ ciDi inh) wh

	(contextArgs, contextArgsTv) = case ciDefType inh of
		TT funList -> unzip $ map (\(typ, x) -> (CppVar (cppType typ) x $ CppAtom x, typePolyVars typ)) $ filter (\(_, y) -> isArgContext y) $ zip (init funList) args
		_ -> ([], []) where

	isArgContext a = S.member a $ getSetOfListFreeVars (filter isFunction wh) where
		getSetOfListFreeVars = S.unions . map getDefinitionFreeVars where
			getDefinitionFreeVars (Definition _ args val wh)
				= AG.getExpressionAtoms val `S.union` getSetOfListFreeVars wh `subtractSet` S.fromList (args ++ map defName wh)

traceU x y = y

isFunction (Definition _ args _ _) = not $ null args

symTabTranslator symTab f x = case M.lookup x symTab of
	Just (CppFqMethod prefix) -> prefix ++ "::" ++ x
	Just CppContextMethod -> if f then "impl." ++ x else "hn::bind(impl, &local::" ++ x ++ ")"
	Nothing -> x

getFromWhere wh mf ff = map mf $ filter ff wh

defName (Definition name _ _ _) = name
