{-# LANGUAGE TypeFamilies #-}
module CPP.TypeProducer where

import Data.Functor.Foldable

import HN.TypeTools
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
	_ -> x

cppType :: T -> CppType
cppType x = cata f x where
        f (TF x) = CppTypePrimitive $ cppPrimitiveType x
        f (TTF l) = CppTypeFunction (last l) (init l)
        f (TDF polyType typeArgs) = CppTypePolyInstance (cppPrimitiveType polyType) typeArgs
        f (TUF x) = CppTypePrimitive x
        f (TVF x) = CppTypePrimitive x
        f x = error "unsupported type in TypeProducer.cppType"
