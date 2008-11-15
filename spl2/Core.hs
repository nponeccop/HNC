module Core where

import Intermediate
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

import Debug.Trace
import Visualise
import Utils
import Check3
import CPP.TypeProducer
import HN.SplExport
import Types

-- inherited attributes for Definition
data DefinitionInherited = DefinitionInherited {
	diLevel           :: Int
,	diSymTab          :: M.Map String CppAtomType
,	diFreeVarTypes    :: M.Map String T
}

-- synthesized attributes for Definition
data DefinitionSynthesized = DefinitionSynthesized {
	dsCppDef :: CppDefinition
}

sem_Definition inh self @ (Definition name args val wh)
    = DefinitionSynthesized {
    	dsCppDef = CppFunctionDef {
		    	functionLevel 			= diLevel inh
		    ,	functionIsStatic		= isFunctionStatic self
			,	functionReturnType 		= case cppDefType of CppTypeFunction returnType _ -> returnType ; _ -> cppDefType 
			,	functionContext			= getContext inh symTabWithStatics self
			,	functionName 			= name
			,	functionArgs 			= zipWith CppVarDecl (case cppDefType of CppTypeFunction _ argTypes -> argTypes ; _ -> []) args
			,	functionLocalVars 		= wsLocalVars semWhere
			,	functionRetExpr			= sem_Expression (symTabTranslator symTabWithoutArgsAndLocals) val 	    	
		    }
    } where
    	P (exprOutputTypes, defType) = check (convertDef self) exprFvt
    	exprFvt = ((diFreeVarTypes inh) `subtractMap` localsFvt) `M.union` localsFvt where
    		localsFvt = M.fromList $ map (\arg -> (arg, TV arg)) $ args ++ localsList  	
    	
    	cppDefType = cppType defType
    	-- localsList : semWhere 
    	localsList = map (\(CppVar _ name _ ) -> name) (wsLocalVars semWhere)
    	-- symTabWithStatics : semWhere
      	symTabWithStatics = (wsLocalFunctionMap semWhere) `M.union` (diSymTab inh)   	

		-- symTabWithoutArgsAndLocals : self symTabWithStatics localsList      	
    	symTabWithoutArgsAndLocals = symTabWithStatics `subtractKeysFromMap` args `subtractKeysFromMap` localsList
    	  	
    	semWhere = sem_Where symTabT classPrefix isFunctionStatic exprFvt wh where
	    	classPrefix = CppFqMethod $ name ++ "_impl"
	    	-- symTabT : symTabWithStatics 
    		symTabT = symTabTranslator symTabWithStatics
    
    	isFunctionStatic def  = S.null $ getDefinitionFreeVarsWithoutFqn def where
    	    getDefinitionFreeVarsWithoutFqn def = getDefinitionFreeVars def `subtractSet` M.keysSet (diSymTab inh)
  	
data WhereSynthesized = WhereSynthesized {
	wsLocalVars :: [CppLocalVarDef]
,	wsLocalFunctionMap :: M.Map String CppAtomType
}
    	
sem_Where symTabT classPrefix isFunctionStatic fvt self 
	= WhereSynthesized {
		wsLocalVars = getWhereVars symTabT fvt self
	,	wsLocalFunctionMap = getFunctionMap
	} where 	
		getFunctionMap = M.fromList $ mapPrefix classPrefix isFunctionStatic ++ mapPrefix objPrefix (not . isFunctionStatic) where
			objPrefix = CppContextMethod
			mapPrefix prefix fn = map (\(Definition locName _ _ _) -> (locName, prefix)) $ filter (\x -> isFunction x && (fn x)) self
    
isFunction (Definition _ args _ _) = not $ null args

symTabTranslator symTab f x = case M.lookup x symTab of
	Just (CppFqMethod prefix) -> prefix ++ "::" ++ x
	Just CppContextMethod -> if f then "impl." ++ x else "hn::bind(impl, &local::" ++ x ++ ")" 
	Nothing -> x

getContext inh fqnWithLocals def @ (Definition name args _ wh) = constructJust (null vars && null methods) $ CppContext (diLevel inh) (name ++ "_impl") vars methods where

	-- переменные контекста - это 
	-- аргументы главной функции, свободные в where-функциях
	-- локальные переменные, свободные в where-функциях 
	vars = (filter (\(CppVar _ name _ ) -> not $ S.member name lvn) $ (getWhereVars (symTabTranslator $ diSymTab inh) (diFreeVarTypes inh) wh)) ++ contextArgs   
	methods = getWhereMethods (inh { diLevel = diLevel inh + 1 }) def
	lvn = getLocalVars wh
	contextArgs = map (\x -> CppVar (CppTypePrimitive "unknownCA") x $ CppAtom x) $ filter isArgContext args

	wfv = getWhereFuncFreeVars def
	
	isArgContext a = S.member a wfv 
	
isVar (Definition _ args _ _) = null args
getFromWhere wh mf ff = map mf $ filter ff wh

getWhereVars fqn fvt def = getFromWhere def (transformVarDefinition fqn fvt) isVar
getWhereMethods inh def @ (Definition _ _ _ wh) = getFromWhere wh ((.) dsCppDef $ sem_Definition inh) (not . isVar)
getWhereX wh f = S.fromList $ getFromWhere wh (\(Definition name _ _ _) -> name) f

getWhereVarNames wh = getWhereX wh isVar 
getWhereAtoms wh =  getWhereX wh (const True)

-- локальные переменные - внутри функции
getLocalVars wh = getWhereVarNames wh    

-- контекстные переменные - в impl
getContextVars def = getXVars subtractSet def

-- внешние переменные - в impl.outer
getOuterVars def = getXVars S.intersection def

getXVars fn def = fn (getWhereFreeVars def) (getDefinitionFreeVars def)

-- переменные, свободные в данном where. Могут быть контекстными или внешними
getWhereFreeVars (Definition _ _ _ wh) = getSetOfListFreeVars wh

getWhereFuncFreeVars (Definition _ _ _ wh) = getSetOfListFreeVars (filter isFunction wh)

getSetOfListFreeVars ww = S.unions $ map getDefinitionFreeVars ww


transformVarDefinition fqn fvt def @ (Definition name [] val _) =
	CppVar inferredType name $ sem_Expression fqn val where
		inferredType = cppType $ case check (convertExpr val) fvt of P (_, t) -> t ; N mesg -> T mesg  
	
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

getDefinitionFreeVars def @ (Definition _ args val wh) 
	= (S.union (getExpressionAtoms val) (getSetOfListFreeVars wh)) `subtractSet` (S.fromList args) `subtractSet` (getWhereAtoms wh)
