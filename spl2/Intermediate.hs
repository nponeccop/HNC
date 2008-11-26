module Intermediate where
import qualified Data.Map as M

type Program = [Definition]

data Const      =   ConstString String
                |   ConstInt    Int
                |   ConstReal   Double
                |   ConstChar   Char
                |   ConstBool   Bool
                    deriving(Show, Eq)

data Definition
    =   Definition String [String] Expression [Definition]
    deriving(Eq,Show)

data Expression
    =   Application Expression [Expression]
    |   Atom String
    |   Lambda [String] Expression
    |   Constant Const
    deriving(Eq,Show)

type CppProgram = [CppDefinition]

data CppDefinition
    =   CppFunctionDef
        {
        	functionLevel			:: Int
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
    |	CppPtr CppExpression
    |	CppField CppExpression String
    
data CppAtomType 
	= CppAtomVar				-- a(x), x(a) 
	| CppContextVar				-- a(ctx.x), ctx.x(a)
	| CppContextMethod			-- a(hn::bind(ctx, &local::a), ctx.x(a) 
	| CppFqConst String			-- a(aaa::bbb::x)
	| CppFqMethod String		-- a(&aaa::bbb::x), aaa::bbb::x(a)

data CppLocalVarDef
    =   CppVar CppType String CppExpression
    |   CppStatement CppExpression
    
data CppContext 
	=	CppContext 
		{
			contextLevel 		:: Int
		,	contextTypeName		:: String
		,	contextVars			:: [CppLocalVarDef]
		,	contextMethods		:: CppProgram
		}

data CppVarDecl
    =   CppVarDecl CppType String

data CppType 
	= CppTypePrimitive String
	| CppTypePoly String [CppType]
	| CppTypeFunction CppType [CppType]
