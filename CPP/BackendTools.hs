{-# LANGUAGE LambdaCase #-}
module CPP.BackendTools where

import CPP.Intermediate
import CPP.TypeProducer
import CPP.Visualise

import SPL.Types
import HN.TypeTools
import qualified Data.Set as S

fixTA x atv = showTemplateArgs $ map (show .  f) x where
        f (TV v) | not $ S.member v atv = CppTypePrimitive "hn::unused"
        f x = cppType x

moveQualifierDown = \case
	CppContextVar -> CppCurrentClassVar
	CppContextMethod -> CppCurrentClassMethod
	CppContextMethodStatic -> CppCurrentClassMethodStatic
	CppUpperArgument -> CppParentVar
	CppArgument -> CppUpperArgument
	x -> x

nonStaticReference = \case
	CppUpperArgument -> True
	CppContextVar -> True
	CppContextMethod -> True
	CppCurrentClassVar -> True
	CppCurrentClassMethod -> True
	CppParentVar -> True
	_ -> False


transformArgument atomQualifier name atomType nta = let
		funAmpersand = if isFunctionType atomType then "&" else ""
	in case atomQualifier of
		CppFqMethod prefix -> funAmpersand ++ prefix ++ "::" ++ nta
		CppContextMethodStatic -> funAmpersand ++ "local::" ++ nta
		CppContextMethod -> "hn::bind(impl, &local::" ++ nta ++ ")"
		CppContextVar -> "impl." ++ name
		CppArgument -> name
		CppUpperArgument -> name
		CppCurrentClassVar -> name
		CppCurrentClassMethod -> "hn::bind(*this, &self::" ++ nta ++ ")"
		CppLocal -> name
		CppParentVar -> "parent->" ++ name
		CppCurrentClassMethodStatic -> "&self::" ++ nta
		foo -> error $ "transformArgument:" ++ show foo

makeCppVar False cppType c d = CppVar cppType c d
makeCppVar True cppType name (CppApplication _ [cond, body, init]) = CppWhile cppType name init cond [] body

cppVarName (CppVar _ name _) = name
cppVarName (CppWhile _ name _ _ _ _) = name

transformWhile atomQualifier name atomType nta = case atomQualifier of
		CppFqMethod prefix -> prefix ++ "::" ++ name
		CppContextMethodStatic -> "local::" ++ name
		CppContextMethod -> "impl." ++ name
		CppContextVar -> "impl." ++ name
		CppArgument -> name
		CppUpperArgument -> name
		CppCurrentClassVar -> name
		CppCurrentClassMethod -> "hn::bind(*this, &self::" ++ nta ++ ")"
		CppLocal -> name
		CppParentVar -> "parent->" ++ name
		CppCurrentClassMethodStatic -> "&self::" ++ nta
		foo -> error $ "transformArgument:" ++ show foo


transformFunction atomQualifier name atomType xnta = let
		nta = case atomType of
			TT x -> if hasFunctionalType x || cppCannotInferReturnType x
				then xnta
				else name
			_ -> error "Typechecker is wrong - only function type can be at function position"
	in case atomQualifier of
		CppFqMethod prefix -> prefix ++ "::" ++ nta
		CppContextMethod -> "impl." ++ name
		CppContextMethodStatic -> "local::" ++ nta
		CppArgument -> name
		CppLocal -> name
		CppCurrentClassMethod -> name
		CppCurrentClassMethodStatic -> nta
		CppUpperArgument -> name
		foo -> error $ "transformFunction:" ++ show foo
