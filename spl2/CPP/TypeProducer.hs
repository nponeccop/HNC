module CPP.TypeProducer where

import SPL.Types
import Utils
import CPP.Intermediate

cppPrimitiveType x = case x of
	"num" -> "int"
	"string" -> "std::string"
	"boolean" -> "bool"
	"list" -> "std::list"
	_ -> "<<<<Type inference error or unknown primitive type: " ++ x ++ ">>>>"

cppType (T x) = CppTypePrimitive $ cppPrimitiveType x  
	
cppType (TT l) = CppTypeFunction (last cppL) (init cppL) where
	cppL = map cppType l
	
cppType (TD polyType typeArgs) = CppTypePoly (cppPrimitiveType polyType) $ map cppType typeArgs 

cppType x = CppTypePrimitive $ "unknown<" ++ show x ++ ">"

