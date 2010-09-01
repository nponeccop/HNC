module CPP.BackendTools where

import Data.List
import Data.Maybe
import qualified Data.Set as S

import Utils

import CPP.Intermediate
import CPP.TypeProducer
import CPP.Visualise

import SPL.Types
import MilnerTools

fixTA x = makeArgs $ map (show . cppType) x

makeArgs [] = ""
makeArgs x = showTemplateArgs x

moveQualifierDown x = case x of
	CppContextVar -> CppCurrentClassVar
	CppContextMethod -> CppCurrentClassMethod
	CppContextMethodStatic -> CppCurrentClassMethodStatic
	CppUpperArgument -> CppParentVar
	_ -> x

nonStaticReference (_, x) = case x of
	CppUpperArgument -> True
	CppContextVar -> True
	CppContextMethod ->  True
	CppCurrentClassVar -> True
	CppCurrentClassMethod -> True
	CppParentVar -> True
	_ -> False

isFunctionType (TT _) = True
isFunctionType _ = False

isPolymorphicFunctionType x = isFunctionType x && not (S.null $ typeTu x)

hasFunctionalType x = isJust $ find isFunctionType $ init x

transformArgument atomQualifier name atomType templ = let
		funAmpersand = if isFunctionType atomType then "&" else ""
	in case atomQualifier of
		CppFqMethod prefix -> funAmpersand ++ prefix ++ "::" ++ name ++ xtrace "TransformArg" templ
		CppContextMethodStatic -> funAmpersand ++ "local::" ++ name ++ templ
		CppContextMethod -> "hn::bind(impl, &local::" ++ name ++ templ ++ ")"
		CppContextVar -> "impl." ++ name
		CppArgument -> name
		CppUpperArgument -> name
		CppCurrentClassVar -> name
		CppCurrentClassMethod -> "hn::bind(*this, &self::" ++ name ++ templ ++ ")"
		CppLocal -> name
		CppParentVar -> "parent->" ++ name
		CppCurrentClassMethodStatic -> "&self::" ++ name ++ templ
		foo -> error $ "transformArgument:" ++ show foo

cppCannotInferReturnType x = not $ S.null $ (typeTu $ last x) S.\\ (typeTu $ TT $ init x)

transformFunction atomQualifier name atomType templateArgs = let
		ta = case atomType of
			TT x -> if hasFunctionalType x || cppCannotInferReturnType x
				then templateArgs
				else ""
			_ -> error "Typechecker is wrong - only function type can be at function position"

	in case atomQualifier of
		CppFqMethod prefix -> prefix ++ "::" ++ name ++ ta
		CppContextMethod -> "impl." ++ name
		CppContextMethodStatic -> "local::" ++ name ++ ta
		CppArgument -> name
		CppLocal -> name
		CppCurrentClassMethod -> name
		CppCurrentClassMethodStatic -> name ++ ta
		CppUpperArgument -> name
		foo -> error $ "transformFunction:" ++ show foo
