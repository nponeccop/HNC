module CPP.Visualise where

import CPP.Intermediate
import HN.Intermediate
import Utils

showWithIndent indentationLevel y
	= concat $ map (inStrings indent "\n") $ filter (not . null) y where
		indent = replicate indentationLevel '\t'

showFunctionPrototype def = show (functionReturnType def) ++ " " ++ functionName def ++ inParens (showFunctionArgs $ functionArgs def)

inParens x = inStrings "(" ")" x
inAngular x = inStrings "<" ">" x

showFunctionArgs l = showJoinedList ", " l

joinComma = joinStr ", "

showTemplateArgs [] = ""
showTemplateArgs typeArgs = inAngular $ joinComma typeArgs

ifNotNull l res = if null l then [] else res

getTemplateDecl templateArgs = ifNotNull templateArgs [templateDecl] where
	templateDecl = "template "  ++ showTemplateArgs (map ("typename " ++) templateArgs)

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
	show def @ (CppFunctionDef level templateArgs isStatic context _ _ _ localVars retVal)
		= maybe [] show context ++ showWithIndent level (concat
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
			getContextInit vars
				= "\tlocal impl = { " ++ initVars ++ " };"
				where initVars = joinComma $ map (\(CppVar _ _ v) -> show v) vars
			showContextInit (CppContext _ templateVars tn vars _)
				= ("\ttypedef " ++ tn ++ showTemplateArgs templateVars ++ " local;") : ifNotNull vars [getContextInit vars]

instance Show CppLocalVarDef where
    show (CppVar a b c) = show a ++ " " ++ b ++ " = " ++ show c ++ ";"

instance Show CppVarDecl where
	show (CppVarDecl typ name) = show typ ++ " " ++ name

showAtomApplication "ff::_if" [cond, branch1, branch2]
	= show cond ++ " ? " ++ show branch1 ++ " : " ++ show branch2

showAtomApplication "ff::_or" a = infixOp "||" a
showAtomApplication "ff::eq" a = infixOp "==" a
showAtomApplication "ff::mod" a = infixOp "%" a
showAtomApplication "ff::sum" a = infixOp "+" a
showAtomApplication "ff::mul" a = infixOp "*" a
showAtomApplication "ff::div" a = infixOp "/" a
showAtomApplication "ff::incr" [a] = show a ++ " + 1";

showAtomApplication a b = a ++ inParens (showFunctionArgs b)

infixOp op [arg1, arg2] = show arg1 ++  " " ++ op ++ " " ++ show arg2;

instance Show CppExpression where
	show (CppLiteral l) = case l of
		(ConstString s) -> show s
		(ConstInt s) -> show s
	show (CppAtom l) = l

	show (CppApplication (CppAtom a) b) =
		showAtomApplication a b

	show (CppApplication a b)
		= inParens (show a) ++ inParens (showFunctionArgs b)

instance Show CppType where
	show (CppTypePrimitive p)
		= p
	show (CppTypeFunction ret args)
		= "boost::function" ++ inAngular (show ret ++ " " ++ inParens (showFunctionArgs args))
	show (CppTypePolyInstance polyType typeArgs)
		= polyType ++ inAngular (showFunctionArgs typeArgs)
