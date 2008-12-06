module CPP.Visualise where

import Maybe

import CPP.Intermediate
import HN.Intermediate
import Utils

mapAndJoinStr _ [] = [] 
mapAndJoinStr x l = foldl1 (++) $ map x l

showJoinedList2 separator = mapAndJoinStr (\x -> (show x) ++ separator)

showWithIndent indentationLevel y
	= concat $ map (inStrings indent "\n") $ filter (\x -> not $ null x) y where
		indent = replicate indentationLevel '\t'

showFunctionPrototype def = (show $ functionReturnType def) ++ " " ++ functionName def ++ (inParens $ showFunctionArgs $ functionArgs def)

inParens x = inStrings "(" ")" x
inAngular x = inStrings "<" ">" x
inStrings l r x = l ++ x ++ r

showFunctionArgs l = showJoinedList ", " l

joinComma = joinStr ", "

showTemplateArgs [] = ""
showTemplateArgs typeArgs = inAngular $ joinComma typeArgs

ifNotNull l res = if null l then [] else res

getTemplateDecl templateArgs = ifNotNull templateArgs [templateDecl] where
	templateDecl = "template "  ++ (showTemplateArgs $ map ((++) "typename ") templateArgs)

instance Show CppContext where
	show (CppContext level templateArgs name vars methods) 
		= let sil = showWithIndent level in concat 
		[
			sil $ concat [
					getTemplateDecl templateArgs
				,  	["struct " ++ name, "{"]
				,	map (\(CppVar t n _) -> inStrings "\t" ";" $ show $ CppVarDecl t n) vars
			]
		,	ifNotNull vars "\n" 
		,	mapAndJoinStr show methods  
		,	sil ["};"] 
		, 	"\n"
		]

instance Show CppDefinition where
	show def @ (CppFunctionDef level templateArgs isStatic context typeName name args localVars retVal) 
		= maybe [] show context ++ (showWithIndent level $ concat $
		[
			getTemplateDecl templateArgs
		, 	[
				(if isStatic && level > 0 then "static " else "") ++ showFunctionPrototype def
			,	"{"
			]
		,	map ((++) "\t" . show) localVars 
		,	maybe [] showContextInit context 
		,	[ 
				inStrings "\treturn " ";" $ show retVal 
			,	"};"
			]
		]) where
			getContextInit (CppContext _ templateVars tn vars _)
				= "\t" ++ tn ++ showTemplateArgs templateVars ++ " impl = { " ++ initVars ++ " };" 
				where initVars = joinComma $ map (\(CppVar _ _ v) -> show v) vars 
			showContextInit ctx @ (CppContext _ _ _ vars _)
				= "\ttypedef main_impl local;" : ifNotNull vars [getContextInit ctx]
   
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
		= "boost::function" ++ (inAngular $ show ret ++ " " ++ (inParens $ showFunctionArgs args))
	show (CppTypePolyInstance polyType typeArgs) 
		= polyType ++ (inAngular $ showFunctionArgs typeArgs)
