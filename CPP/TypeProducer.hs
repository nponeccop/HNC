{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

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
cppType = cata $ \case
	TF x -> CppTypePrimitive $ cppPrimitiveType x
	TTF l -> CppTypeFunction (last l) (init l)
	TDF polyType typeArgs -> CppTypePolyInstance (cppPrimitiveType polyType) typeArgs
	TUF x -> CppTypePrimitive x
	TVF x -> CppTypePrimitive x
	_ -> error "unsupported type in TypeProducer.cppType"
