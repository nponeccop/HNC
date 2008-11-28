module CPP.TypeProducer where

import Data.List
import qualified Data.Set as S

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
	isPoly = not $ null templateArgs
	templateArgs = S.toList $ typePolyVars (TT l)  
	
cppType (TD polyType typeArgs) = CppTypePolyInstance (cppPrimitiveType polyType) $ map cppType typeArgs

cppType (TU x) = CppTypePrimitive x

cppType x = CppTypePrimitive $ "unknown<" ++ show x ++ ">"

isTypePolymorphic (T _) = False
isTypePolymorphic (TT l) = maybe False (const True) $ find isTypePolymorphic l 
isTypePolymorphic (TU _) = True

typePolyVars (TT l) = S.unions $ map typePolyVars l
typePolyVars (TU v) = S.singleton v
typePolyVars _ = S.empty
