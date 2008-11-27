module CPP.Core where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Maybe
import Debug.Trace

import HN.Intermediate
import HN.SplExport

import CPP.Intermediate
import CPP.Visualise
import CPP.TypeProducer
import Utils

import SPL.Check3
import SPL.Types


-- inherited attributes for Definition
data DefinitionInherited = DefinitionInherited {
	diLevel           :: Int
,	diSymTab          :: M.Map String CppAtomType
,	diFreeVarTypes    :: M.Map String T
,	diType			  :: Maybe CppType
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
			,	functionContext			= getContext (wsMethods semWhere) finalFvt inh symTabWithStatics exprOutputTypes defType self
			,	functionName 			= name
			,	functionArgs 			= zipWith CppVarDecl (case cppDefType of CppTypeFunction _ argTypes -> argTypes ; _ -> []) args
			,	functionLocalVars 		= wsLocalVars semWhere
			,	functionRetExpr			= sem_Expression (symTabTranslator symTabWithoutArgsAndLocals) val 	    	
		    }
    } where
    
--	vars = map (\(CppVar _ name val ) -> CppVar (cppType $ uncondLookup name whereTypes) name val) $ trace2 vars1
    
    	finalFvt = exprFvt
    	whereNames = map (\(Definition name _ _ _) -> name) wh
    	P (exprOutputTypes, defType) = check (convertDef self) exprFvt whereNames
    	exprFvt = ((diFreeVarTypes inh) `subtractMap` localsFvt) `M.union` localsFvt where
    		localsFvt = M.fromList $ map (\arg -> (arg, TV arg)) $ args ++ localsList
 	
    	cppDefType = maybe (cppType defType) id $ diType inh
    	-- localsList : semWhere 
    	localsList = map (\(CppVar _ name _ ) -> name) (wsLocalVars semWhere)
    	-- symTabWithStatics : semWhere
      	symTabWithStatics = (wsLocalFunctionMap semWhere) `M.union` (diSymTab inh)   	

		-- symTabWithoutArgsAndLocals : self symTabWithStatics localsList      	
    	symTabWithoutArgsAndLocals = symTabWithStatics `subtractKeysFromMap` args `subtractKeysFromMap` localsList
    	  	
    	semWhere = sem_Where (WhereInherited symTabT classPrefix isFunctionStatic exprFvt exprOutputTypes inh { diLevel = diLevel inh + 1, diFreeVarTypes = finalFvt }) wh where
	    	classPrefix = CppFqMethod $ name ++ "_impl"
	    	-- symTabT : symTabWithStatics 
    		symTabT = symTabTranslator symTabWithStatics
    
    	isFunctionStatic def  = S.null $ getDefinitionFreeVarsWithoutFqn def where
    	    getDefinitionFreeVarsWithoutFqn def = getDefinitionFreeVars def `subtractSet` M.keysSet (diSymTab inh)
  	
data WhereSynthesized d = WhereSynthesized {
	wsLocalVars :: [CppLocalVarDef]
,	wsLocalFunctionMap :: M.Map String CppAtomType
,	wsFvt :: M.Map String T
,	wsMethods :: d
}

data WhereInherited a b c d e f = WhereInherited {
	wiSymTabT          :: a
,	wiClassPrefix      :: b
,	wiIsFunctionStatic :: c
,	wiFvt              :: d
,	wiTypes            :: f
,	wiDi			   :: e
}
    	
sem_Where inh self 
	= WhereSynthesized {
		wsLocalVars = getWsLocalVars inh self
	,	wsLocalFunctionMap = getWsLocalFunctionMap inh self
	,	wsFvt = getWsFvt inh self
	,	wsMethods = getWhereMethods (wiDi inh) (wiTypes inh) self
	} 
	
		
getWsLocalVars inh self = getWhereVars (wiSymTabT inh) (wiFvt inh) self
		
getWsLocalFunctionMap inh self = M.fromList $ mapPrefix (wiClassPrefix inh) (wiIsFunctionStatic inh) ++ mapPrefix objPrefix (not . wiIsFunctionStatic inh) where
		objPrefix = CppContextMethod
		mapPrefix prefix fn = map (\(Definition locName _ _ _) -> (locName, prefix)) $ filter (\x -> isFunction x && (fn x)) self
			
getWsFvt inh self = wiFvt inh
    
isFunction (Definition _ args _ _) = not $ null args

symTabTranslator symTab f x = case M.lookup x symTab of
	Just (CppFqMethod prefix) -> prefix ++ "::" ++ x
	Just CppContextMethod -> if f then "impl." ++ x else "hn::bind(impl, &local::" ++ x ++ ")" 
	Nothing -> x
	
getContext methods fvt inh fqnWithLocals whereTypes defType def @ (Definition name args _ wh) = constructJust (null vars && null methods) $ CppContext (diLevel inh) (name ++ "_impl") vars methods where

	-- ���������� ��������� - ��� 
	-- ��������� ������� �������, ��������� � where-��������
	-- ��������� ����������, ��������� � where-�������� 
	vars = (filter (\(CppVar _ name _ ) -> not $ S.member name lvn) $ (getWhereVars (symTabTranslator $ diSymTab inh) (diFreeVarTypes inh) wh))  ++ contextArgs   
		
	lvn = getLocalVars wh
	
	contextArgs = case defType of
		TT funList -> map (\(typ, x) -> CppVar (cppType typ) x $ CppAtom x) $ filter (\(x, y) -> isArgContext y) $ zip (init funList) args
		_ -> []
	
	getCppType x = cppType $ fromJust $ M.lookup x fvt 

	wfv = getWhereFuncFreeVars def
	
	isArgContext a = S.member a wfv 
	
isVar (Definition _ args _ _) = null args
getFromWhere wh mf ff = map mf $ filter ff wh

getWhereVars fqn fvt def = getFromWhere def (sem_VarDefinition fqn fvt) isVar

getWhereMethods inh whereTypes wh = getFromWhere wh (\def -> dsCppDef $ sem_Definition (f def) def) (not . isVar) 
	where
		f def = inh { diType = Just $ cppType $ uncondLookup (defName def) whereTypes }
		defName (Definition name _ _ _ ) = name


getWhereX wh f = S.fromList $ getFromWhere wh (\(Definition name _ _ _) -> name) f

getWhereVarNames wh = getWhereX wh isVar 
getWhereAtoms wh =  getWhereX wh (const True)

-- ��������� ���������� - ������ �������
getLocalVars wh = getWhereVarNames wh    

-- ����������� ���������� - � impl
getContextVars def = getXVars subtractSet def

-- ������� ���������� - � impl.outer
getOuterVars def = getXVars S.intersection def

getXVars fn def = fn (getWhereFreeVars def) (getDefinitionFreeVars def)

-- ����������, ��������� � ������ where. ����� ���� ������������ ��� ��������
getWhereFreeVars (Definition _ _ _ wh) = getSetOfListFreeVars wh

getWhereFuncFreeVars (Definition _ _ _ wh) = getSetOfListFreeVars (filter isFunction wh)

getSetOfListFreeVars ww = S.unions $ map getDefinitionFreeVars ww


sem_VarDefinition fqn fvt def @ (Definition name [] val _) =
	CppVar inferredType name $ sem_Expression fqn val where
		inferredType = cppType $ case check (convertExpr val) fvt [] of P (_, t) -> t ; N mesg -> T mesg  
	
sem_Expression fqn p = case p of
	Atom x -> CppAtom $ fqn False x
	Constant x -> CppLiteral x
	-- ���� hn::bind(impl, &main_impl::a) - CppApplication CppAtom "hn::bind" [ CppAtom "impl", CppPtr (CppAtom a) ] 
	-- ���� impl.a - CppField impl a
	-- ���� &main_impl::a - CppPtr (CppAtom a)
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