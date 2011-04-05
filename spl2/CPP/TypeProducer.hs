module CPP.TypeProducer where

import SPL.Types
import CPP.Intermediate

cppPrimitiveType x = case x of
	"num" -> "int"
	"string" -> "std::string"
	"boolean" -> "bool"
	"list" -> "std::list"
	"void" -> "void"
	"IO" -> "ff::IO"
	"udp_socket" -> "ff::UdpSocket"
	"pair" -> "std::pair"
	"ptr" -> "ff::ptr"
	_ -> "<<<<Type inference error or unknown primitive type: " ++ x ++ ">>>>"

cppType (T x) = CppTypePrimitive $ cppPrimitiveType x

cppType (TT l) = CppTypeFunction (last cppL) (init cppL) where
	cppL = map cppType l

cppType (TD polyType typeArgs) = CppTypePolyInstance (cppPrimitiveType polyType) $ map cppType typeArgs

cppType (TU x) = CppTypePrimitive x
cppType (TV x) = cppType (TU x)

cppType x = CppTypePrimitive $ "unknown<" ++ show x ++ ">"
