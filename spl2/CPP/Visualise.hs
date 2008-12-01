module CPP.Visualise where

import Maybe

import CPP.Intermediate
import HN.Intermediate
import Utils

mapAndJoinStr _ [] = [] 
mapAndJoinStr x l = foldl1 (++) $ map x l

showJoinedList2 separator = mapAndJoinStr (\x -> (show x) ++ separator)

showProgram l = showJoinedList "\n\n" l

showWithIndent indentationLevel y
	= joinStr "" $ (map (inStrings indent "\n") $ filter (\x -> not $ null x) y) where
		indent = replicate indentationLevel '\t'

showFree y = mapAndJoinStr (inStrings "\t" ";" . show) y

showFunctionPrototype def = (show $ functionReturnType def) ++ " " ++ functionName def ++ (inParens $ showFunctionArgsWithTypes $ functionArgs def)

inParens x = inStrings "(" ")" x
inCurly x = inStrings "{" "}" x
inAngular x = inStrings "<" ">" x
inStrings l r x = l ++ x ++ r

showFunctionArgs l = showJoinedList ", " l

showFunctionArgsWithTypes args = joinStr ", " $ map show args

getTemplateDecl templateArgs = if null templateArgs then [] else [templateDecl] where
	templateDecl = "template "  ++ (inAngular $ joinStr ", " $ map (\x -> "typename " ++ x) templateArgs)

instance Show CppContext where
	show (CppContext level templateArgs name vars methods) 
		=
				(sil $ getTemplateDecl templateArgs  ++	["struct " ++ name, "{"]
		++ (map (\(CppVar t n _) -> inStrings "\t" ";" $ show $ CppVarDecl t n) vars)) 
		++ (if null vars then "" else "\n") 
		++ (mapAndJoinStr show methods)  
		++ sil ["};"] 
		++ "\n"  where
			sil = showWithIndent level

instance Show CppDefinition where
	show def @ (CppFunctionDef level templateArgs isStatic context typeName name args localVars retVal) 
		= (if isNothing context then "" else show $ fromJust context) ++ (showWithIndent level $
		
		getTemplateDecl templateArgs
		++
		[
			(if isStatic then "static " else "") ++ showFunctionPrototype def
		,	"{"
		] 
		++ map (\x -> "\t" ++ show x) localVars 
		++ showContextInit 
		++ 
		[ 
			inStrings "\treturn " ";" $ show retVal 
		,	"};" 
		]) where
			getContextInit templateVars tn vars = "\t" ++ tn ++ " impl" ++ showTemplateArgs templateVars ++ " = { " ++ initVars ++ " };" 
				where initVars = joinStr ", " $ map (\(CppVar _ _ v) -> show v) vars 
			showContextInit = 
				case context of 
					Nothing -> []
					Just (CppContext _ _ _ [] _) -> [ contextTypeDef ]
					Just (CppContext _ templateVars tn vars _) -> [ contextTypeDef, getContextInit templateVars tn vars ]
			contextTypeDef = "\ttypedef main_impl local;"
			
			showTemplateArgs [] = ""
			showTemplateArgs typeArgs = inAngular $ joinStr ", " typeArgs
			 
   
instance Show CppLocalVarDef where
    show (CppVar a b c) = show a ++ " " ++ b ++ " = " ++ show c ++ ";"
    
instance Show CppVarDecl where
	show (CppVarDecl typ name) = show typ ++ " " ++ name
    
instance Show CppExpression where
    show (CppLiteral l) = case l of 
    		(ConstString s) -> show s
    		(ConstInt s) -> show s
    show (CppAtom l) = l
    show (CppApplication (CppAtom a) b)
        = a ++ (inParens $ showFunctionArgs b)
    show (CppApplication a b)
        = (inParens $ show a) ++ (inParens $ showFunctionArgs b)

instance Show CppType where
	show (CppTypePrimitive p) 
		= p 
	show (CppTypeFunction ret args) 
		= "boost::function" ++ (inAngular $ show ret ++ " (*)" ++ (inParens $ showFunctionArgs args))
	show (CppTypePolyInstance polyType typeArgs) 
		= polyType ++ (inAngular $ showFunctionArgs typeArgs)
	