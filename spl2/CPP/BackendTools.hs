module CPP.BackendTools where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Utils

import CPP.Intermediate
import CPP.TypeProducer
import CPP.Visualise

import SPL.Types

xsubstitute m x = case M.lookup x m of
	Nothing -> x
	Just t -> show $ cppType t

makeArgs [] = ""
makeArgs x = showTemplateArgs x

moveQualifierDown x = case x of
	CppContextVar -> CppCurrentClassVar
	CppContextMethod -> CppCurrentClassMethod
	CppContextMethodStatic -> CppCurrentClassMethodStatic
	_ -> x

mapParent x = M.map f x where
	f x = case x of
		CppContextVar -> CppParentVar
		CppUpperArgument -> CppParentVar
		y -> y

transformArgument symTab name callSiteType visibleAtoms templ = let
		funAmpersand = case callSiteType of
			TT _ -> "&"
			_ -> ""
	in case M.lookup name symTab of
		Just (CppFqMethod prefix) -> funAmpersand ++ prefix ++ "::" ++ name ++ xtrace "TransformArg" templ
		Just CppContextMethodStatic -> funAmpersand ++ "local::" ++ name ++ templ
		Just CppContextMethod -> "hn::bind(impl, &local::" ++ name ++ templ ++ ")"
		Just CppContextVar -> "impl." ++ name
		Just CppArgument -> name
		Just CppUpperArgument -> name
		Just CppCurrentClassVar -> name
		Just CppCurrentClassMethod -> "hn::bind(*this, &self::" ++ name ++ templ ++ ")"
		Just CppLocal -> name
		Just CppParentVar -> "parent->" ++ name
		Just CppCurrentClassMethodStatic -> "&self::" ++ name ++ templ
		Just foo -> error $ "transformArgument:" ++ show foo
		Nothing -> xtrace "transformArgument.Nothing" $ funAmpersand ++ name

transformFunction symTab name callSiteType visibleAtoms templateArgs = let
		ta = case atomType of
			(TT x) | (||) (ttt x) $ not $ S.null $ subtractSet (xtrace "AG.transformFunction.pv1" $ typeAllPolyVars $ last x) (xtrace "AG.transformFunction.pv2" $ typeAllPolyVars (TT $ init x)) -> templateArgs
			_ -> ""
		atomType = xtrace "AG.transformFunction.atomType" $ uncondLookup name visibleAtoms
		ttt x = isJust $ find (\x -> isFunctionType x && (not $ S.null $ typePolyVars x)) x
		isFunctionType (TT _) = True
		isFunctionType _ = False
	in case M.lookup name symTab of
		Just (CppFqMethod prefix) -> prefix ++ "::" ++ name ++ ta
		Just CppContextMethod -> "impl." ++ name
		Just CppContextMethodStatic -> "local::" ++ name ++ ta
		Just CppArgument -> name
		Just CppLocal -> name
		Just CppCurrentClassVar -> name
		Just CppCurrentClassMethod -> name
		Just CppCurrentClassMethodStatic -> "self::" ++ name ++ ta
		Just CppUpperArgument -> name
		Just foo -> error $ "transformFunction:" ++ show foo
		Nothing -> name
