module CPP.BackendTools where

import CPP.Intermediate
import CPP.TypeProducer
import CPP.Visualise

import SPL.Types
import HN.TypeTools

fixTA x = showTemplateArgs $ map (show . cppType) x

moveQualifierDown x = case x of
	CppContextVar -> CppCurrentClassVar
	CppContextMethod -> CppCurrentClassMethod
	CppContextMethodStatic -> CppCurrentClassMethodStatic
	CppUpperArgument -> CppParentVar
	CppArgument -> CppUpperArgument
	_ -> x

nonStaticReference x = case x of
	CppUpperArgument -> True
	CppContextVar -> True
	CppContextMethod ->  True
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
