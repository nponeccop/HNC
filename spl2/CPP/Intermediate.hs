module CPP.Intermediate where

import HN.Intermediate

type CppProgram = [CppDefinition]

data CppDefinition
    =   CppFunctionDef
        {
        	functionLevel			:: Int
		,   functionTemplateArgs	:: [String]
        ,	functionIsStatic		:: Bool
		,  	functionContext			:: Maybe CppContext
        ,	functionReturnType		:: CppType
        ,	functionName        	:: String
        ,   functionArgs    		:: [CppVarDecl]
        ,   functionLocalVars       :: [CppLocalVarDef]
        ,   functionRetExpr         :: CppExpression
        }

data CppExpression
    =   CppApplication CppExpression [CppExpression]
    |   CppAtom String
    |   CppLiteral Const
--    |	CppPtr CppExpression
--    |	CppField CppExpression String

data CppAtomType
	= CppAtomVar				-- a(x), x(a)
	| CppContextVar				-- a(ctx.x), ctx.x(a)
	| CppContextMethod			-- a(hn::bind(ctx, &local::a), ctx.x(a)
	| CppFqConst String			-- a(aaa::bbb::x)
	| CppFqMethod String		-- a(&aaa::bbb::x), aaa::bbb::x(a)

data CppLocalVarDef
    =   CppVar CppType String CppExpression
--    |   CppStatement CppExpression

data CppContext
	=	CppContext
		{
			contextLevel 		:: Int
		,	contextTemplateArgs :: [String]
		,	contextTypeName		:: String
		,	contextVars			:: [CppLocalVarDef]
		,	contextMethods		:: CppProgram
		}

data CppVarDecl
    =   CppVarDecl CppType String

data CppType
	= CppTypePrimitive String
	| CppTypePolyInstance String [CppType]
	| CppTypeFunction CppType [CppType]
