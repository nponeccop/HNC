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
			functionContext			=  fmap (\ctt -> ctt { contextMethods = wsMethods semWhere }) agContext
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

		agContext = functionContext $ AG.cppDefinition_Syn_Definition semDef

		semWhere = sem_Where wh WhereInherited {
					wiSymTabT          = symTabTranslator symTabWithStatics
				,	wiTypes            = AG.deconstructTyped $ diTyped inh
				,	wiDi               = inh { diLevel = AG.level_Inh_Definition agInh + 1 }
				}

		symTabWithStatics = lfm (wsMapPrefix semWhere) `M.union` diSymTab inh

		lfm mapPrefix = M.fromList $ mapPrefix classPrefix isFunctionStatic ++ mapPrefix CppContextMethod (not . isFunctionStatic) where
			classPrefix = CppFqMethod $ contextTypeName (fromJust agContext) ++ showTemplateArgs (contextTemplateArgs $ fromJust agContext)

		isFunctionStatic def  = S.null $ (AG.freeVars_Syn_Definition xsemDef) `subtractSet` M.keysSet (diSymTab inh) where
			xsemDef = AG.wrap_Definition (AG.sem_Definition def) agInh

data WhereSynthesized d e f = WhereSynthesized {
	wsMethods :: d
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
	,   wsMapPrefix = mapPrefix
	} where
		wsMethods1   = sem_WhereMethods (wiDi inh)      (diTyped $ wiDi inh) self
		mapPrefix prefix fn = map (\def -> (defName def, prefix)) $ filter (\x -> isFunction x && fn x) self


sem_WhereMethods inh whereTyped wh = map mf typedMethods where
	typedDef = zip wh (AG.unfoldTyped $ diTyped inh)
	typedMethods = filter (isFunction . fst) typedDef
	mf (method, (t, ww)) = dsCppDef $ sem_Definition newInh method where
		newInh = inh { diInferredType = t, diTyped = ww }

traceU x y = y

isFunction (Definition _ args _ _) = not $ null args

symTabTranslator symTab f x = case M.lookup x symTab of
	Just (CppFqMethod prefix) -> prefix ++ "::" ++ x
	Just CppContextMethod -> if f then "impl." ++ x else "hn::bind(impl, &local::" ++ x ++ ")"
	Nothing -> x

getFromWhere wh mf ff = map mf $ filter ff wh

defName (Definition name _ _ _) = name
