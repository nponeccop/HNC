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

transformArgument symTab name callSiteType visibleAtoms templ = let
		funAmpersand = case callSiteType of
			TT _ -> "&"
			_ -> ""
	in case M.lookup name symTab of
		Just (CppFqMethod prefix) -> funAmpersand ++ prefix ++ "::" ++ name ++ xtrace "TransformArg" templ
		Just CppContextMethod -> "hn::bind(impl, &local::" ++ name ++ ")"
		Just CppContextVar -> "impl." ++ name
		Nothing -> funAmpersand ++ name

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
		Nothing -> name


