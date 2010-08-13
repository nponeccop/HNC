module CPP.TypeProducer where

import qualified Data.Set as S

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
	_ -> "<<<<Type inference error or unknown primitive type: " ++ x ++ ">>>>"

cppType (T x) = CppTypePrimitive $ cppPrimitiveType x

cppType (TT l) = CppTypeFunction (last cppL) (init cppL) where
	cppL = map cppType l

cppType (TD polyType typeArgs) = CppTypePolyInstance (cppPrimitiveType polyType) $ map cppType typeArgs

cppType (TU x) = CppTypePrimitive x
cppType (TV x) = cppType (TU x)

cppType (TUL x) = cppType $ head x

cppType x = CppTypePrimitive $ "unknown<" ++ show x ++ ">"

uncurryFunctionType [argType] [] = [argType]
uncurryFunctionType argTypes [] = [TT argTypes]
uncurryFunctionType (ht : tt) (_ : ta) = ht : uncurryFunctionType tt ta
uncurryFunctionType [] _ = error "CPP.TypeProducer.uncurryFunctionType encountered []"

cppUncurryType (TT argTypes) args = cppType $ TT $ uncurryFunctionType argTypes args
cppUncurryType splType _ = cppType splType

typePolyVars x = let union = S.unions . map typePolyVars in case x of
	TU v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty

typeAllPolyVars x = let union = S.unions . map typeAllPolyVars in case x of
	TU v -> S.singleton v
	TV v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty

typeTv x = let union = S.unions . map typeTv in case x of
		TV v -> S.singleton v
		TT l -> union l
		TD _ l -> union l
		_    -> S.empty
