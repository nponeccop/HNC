module CPP.TypeProducer where

import Data.List
import qualified Data.Set as S

import SPL.Types
import CPP.Intermediate

import Utils

cppPrimitiveType x = case x of
	"num" -> "int"
	"string" -> "std::string"
	"boolean" -> "bool"
	"list" -> "std::list"
	_ -> "<<<<Type inference error or unknown primitive type: " ++ x ++ ">>>>"

cppType (T x) = CppTypePrimitive $ cppPrimitiveType x  
	
cppType (TT l) = CppTypeFunction (last cppL) (init cppL) where
	cppL = map cppType l
	isPoly = not $ null templateArgs
	templateArgs = S.toList $ typePolyVars (TT l)  
	
cppType (TD polyType typeArgs) = CppTypePolyInstance (cppPrimitiveType polyType) $ map cppType typeArgs

cppType (TU x) = CppTypePrimitive x

cppType x = CppTypePrimitive $ "unknown<" ++ show x ++ ">"

uncurryFunctionType [] [] = []
uncurryFunctionType [argType] [] = [argType]
uncurryFunctionType argTypes [] = [TT argTypes]
uncurryFunctionType (ht : tt) (_ : ta) = ht : uncurryFunctionType tt ta 

cppUncurryType (TT argTypes) args = cppType $ TT $ uncurryFunctionType argTypes args  
cppUncurryType splType _ = cppType splType 

typePolyVars x = (case x of
	TU v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty)
		where
			union = S.unions . map typePolyVars 
