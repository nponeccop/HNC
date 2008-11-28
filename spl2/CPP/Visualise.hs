module CPP.Visualise where

import Maybe

import CPP.Intermediate
import HN.Intermediate
import Utils
import CPP.TypeProducer

mapAndJoinStr _ [] = [] 
mapAndJoinStr x l = foldl1 (++) $ map x l

showJoinedList2 separator = mapAndJoinStr (\x -> (show x) ++ separator)

showProgram l = showJoinedList "\n\n" l

showWithIndent indentationLevel y
	= joinStr "" $ (map (\x -> indent ++ x ++ "\n") $ filter (\x -> not $ null x) $ y) where
		indent = replicate indentationLevel '\t'

showFree y = mapAndJoinStr (\x -> "\t" ++ (show x) ++ ";") y

showFunctionPrototype def = show (functionReturnType def) ++ " " ++ (functionName def) ++ "(" ++ (showFunctionArgsWithTypes $ functionArgs def) ++ ")"

getTemplateDecl templateArgs = "template <"  ++ (joinStr ", " $ map (\x -> "typename " ++ x) templateArgs) ++ ">"

instance Show CppContext where
	show (CppContext level name vars methods) = 
		sil ["struct " ++ name, "{"]
		++ sil (map (\(CppVar t n _) -> "\t" ++ (show $ CppVarDecl t n) ++ ";") vars) 
		++ (if null vars then "" else "\n") 
		++ (mapAndJoinStr show methods)  
		++ sil ["};"] 
		++ "\n"  where
			sil = showWithIndent level

instance Show CppDefinition where
	show def @ (CppFunctionDef level templateArgs isStatic context typeName name args localVars retVal) 
		= (if isNothing context then "" else show $ fromJust context) ++ (showWithIndent level $
		
		(if (null templateArgs) then [] else [getTemplateDecl templateArgs])
		++
		[
			(if isStatic then "static " else "") ++ showFunctionPrototype def
		,	"{"
		] 
		++ map (\x -> "\t" ++ show x) localVars 
		++ showContextInit 
		++ 
		[ 
			"\treturn " ++  show retVal ++ ";" 
		,	"};" 
		]) where
			getContextInit tn vars = "\t" ++ tn ++ " impl = { " ++ initVars ++ " };" 
				where initVars = joinStr ", " $ map (\(CppVar _ _ v) -> show v) vars 
			showContextInit = 
				case context of 
					Nothing -> []
					Just (CppContext _ _ [] _) -> [ contextTypeDef ]
					Just (CppContext _ tn vars methods) -> [ contextTypeDef, getContextInit tn vars ]
			contextTypeDef = "\ttypedef main_impl local;"
			 
   
instance Show CppLocalVarDef where
    show (CppVar a b c) = show a ++ " " ++ b ++ " = " ++ (show c) ++ ";"
    
instance Show CppVarDecl where
	show (CppVarDecl typ name) = show typ ++ " " ++ name
    
instance Show CppExpression where
    show (CppLiteral l) = case l of 
    		(ConstString s) -> show s
    		(ConstInt s) -> show s
    show (CppAtom l) = l
    show (CppApplication (CppAtom a) b)
        = a ++ "(" ++ showFunctionArgs b ++ ")"
    show (CppApplication a b)
        = "(" ++ (show a) ++ ")(" ++ showFunctionArgs b ++ ")"

instance Show CppType where
	show (CppTypePrimitive p) 
		= p 
	show (CppTypeFunction ret args) 
		= "boost::function<" ++ show ret ++ " (*)(" ++ showFunctionArgs args ++ ")>"
	show (CppTypePolyInstance polyType typeArgs) 
		= polyType ++ "<" ++ (showFunctionArgs typeArgs) ++ ">"
 
showFunctionArgs l = showJoinedList ", " l

showFunctionArgsWithTypes args = joinStr ", " $ map show args
	
	