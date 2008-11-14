module CPP.TypeProducer where

import Types
import Utils
import Intermediate


cppType (T x) = CppTypePrimitive $ case x of
	"num" -> "int"
	"string" -> "std::string"
	_ -> "<<<<Type inference error or unknown primitive type: " ++ x ++ ">>>>" 
	
cppType (TT l) = CppTypeFunction (last cppL) (init cppL) where
	cppL = map cppType l 

cppType x = CppTypePrimitive $ "unknown<" ++ show x ++ ">"

showType (CppTypePrimitive p) = p 
showType (CppTypeFunction ret args) = showType ret ++ (showFunctionArgs $ map showType args) 


showFunctionArgs l = showJoinedList ", " l

showFunctionArgsWithTypes args = joinStr ", " $ map f args where  
	f (CppVarDecl typ name) = (showType typ) ++ " " ++ name