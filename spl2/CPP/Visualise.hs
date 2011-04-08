module CPP.Visualise where

import CPP.Intermediate
import HN.Intermediate
import Utils
import Data.Maybe

showWithIndent indentationLevel y
	= concat $ map (inStrings indent "\n") $ filter (not . null) y where
		indent = replicate indentationLevel '\t'

showFunctionPrototype def = show (functionReturnType def) ++ " " ++ functionName def ++ inParens (showFunctionArgs $ functionArgs def)

showFunctionArgs l = showJoinedList ", " l

joinComma = joinStr ", "

showTemplateArgs [] = ""
showTemplateArgs typeArgs = inAngular $ joinComma typeArgs

ifNotNull l res = if null l then [] else res

getTemplateDecl templateArgs = ifNotNull templateArgs [templateDecl] where
	templateDecl = "template "  ++ showTemplateArgs (map ("typename " ++) templateArgs)

instance Show CppContext where
	show (CppContext level templateArgs name vars methods declareSelf parent)
		= let sil = showWithIndent level in concat
		[
			sil $ concat [
					getTemplateDecl templateArgs
				,  	["struct " ++ name, "{"]
				,  	selfDecl
				,   parentDecl
				,  	map (\(CppVar t n _) -> inStrings "\t" ";" $ show $ CppVarDecl t n) vars
			]
		,	ifNotNull vars "\n"
		,	concatMap show methods
		,	sil ["};"]
		, 	"\n"
		] where
			selfDecl = if declareSelf then [ "\ttypedef " ++ name ++ " self;"  ] else []

			parentDecl = case parent of
				Just parentName -> [ "\t" ++ parentName ++ " *parent;"  ]
				Nothing -> []

instance Show CppDefinition where
	-- monomorphic top-level variables without local variables and context (closure env)
	show def @ (CppFunctionDef level [] True Nothing retType name [] [] retVal) | not (name == "hnMain") =
		showWithIndent level $ [show $ CppVar retType name retVal]
	show def @ (CppFunctionDef level templateArgs isStatic context _ _ _ localVars retVal)
		= maybe [] show context ++ showWithIndent level (concat
		[
			getTemplateDecl templateArgs
		, 	[
				(if isStatic && level > 0 then "static " else "") ++ showFunctionPrototype def
			,	"{"
			]
		,	maybe [] showContextInit context
		,	map ((++) "\t" . show) localVars
		,	[
				inStrings "\treturn " ";" $ show retVal
			,	"};"
			]
		]) where
			getContextInit vars parentName
				= "\tlocal impl = { " ++ initVars ++ " };"
				where
					initVars =  joinComma $ fmap (const "this") parentName `consMaybe` map (\(CppVar _ _ v) -> show v) vars

			showContextInit (CppContext _ templateVars tn vars _ _ parentName)
				= ("\ttypedef " ++ tn ++ showTemplateArgs templateVars ++ " local;") : if null vars && isNothing parentName then [] else [getContextInit vars parentName]

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
