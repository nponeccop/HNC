module Visualise where

import Intermediate
import Maybe

joinStr _ [] = ""
joinStr sep l = foldl1 (\x y -> x ++ sep ++ y) l

mapAndJoinStr _ [] = [] 
mapAndJoinStr x l = foldl1 (++) $ map x l

showJoinedList separator = joinStr separator . map show

showJoinedList2 separator = mapAndJoinStr (\x -> (show x) ++ separator)

showFunctionArgs l = showJoinedList ", " l
showProgram l = showJoinedList "\n\n" l

showWithIndent indentationLevel y
	= joinStr "" $ (map (\x -> indent ++ x ++ "\n") $ filter (\x -> not $ null x) $ y) where
		indent = replicate indentationLevel '\t'

showFree y = mapAndJoinStr (\x -> "\t" ++ (show x) ++ ";") y


instance Show CppContext where
	show (CppContext level name vars methods) = 
		sil ["struct " ++ name, "{"]
		++ sil (map (\(CppVar tn n _) -> "\t" ++ (show $ CppVarDecl tn n) ++ ";") vars) 
		++ (if null vars then "" else "\n") 
		++ (mapAndJoinStr show methods)  
		++ sil ["};"] 
		++ "\n"  where
			sil = showWithIndent level
			sil1 = showWithIndent (level + 1)

instance Show CppDefinition where
	show (CppFunctionDef level isStatic context typeName name args localVars retVal) 
		= (if isNothing context then "" else show $ fromJust context) ++ (showWithIndent level $
		[ 
			(if isStatic then "static " else "") ++ typeName ++ " " ++ name ++ "(" ++ showFunctionArgs args ++ ")"
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
    show (CppVar a b c) = a ++ " " ++ b ++ " = " ++ (show c) ++ ";"
    
instance Show CppVarDecl where
    show (CppVarDecl a b) = a ++ " " ++ b
    
instance Show CppExpression where
    show (CppLiteral l) = case l of (ConstString s) -> show s
    show (CppAtom l) = l
    show (CppApplication (CppAtom a) b)
        = a ++ "(" ++ (showFunctionArgs b) ++ ")"
    show (CppApplication a b)
        = "(" ++ (show a) ++ ")(" ++ (showFunctionArgs b) ++ ")"
