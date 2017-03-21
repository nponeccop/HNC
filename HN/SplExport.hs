{-# LANGUAGE LambdaCase #-}

module HN.SplExport (convertToSpl, convertExpr, convertDef) where

import Data.Functor.Foldable

import HN.Intermediate
import SPL.Visualise (showAsSource)
import SPL.Types
import Utils (joinStr)

convertToSpl = (\x -> show x ++ "\n" ++ joinStr "\n" (map showAsSource x)) . map convertDef

convertExpr :: Expression String -> C
convertExpr = cata $ \case
	ConstantF (ConstInt i) -> CNum i
	ConstantF (ConstString i) -> CStr i
	AtomF a -> CVal a
	ApplicationF a b -> CL a $ K b

convertDef (Definition _ arguments l)
	= (case arguments of
		[] -> convertedWithWhere
		_ -> CL xvalue (S arguments)) where
		xvalue = case whereDefinitions of
			[] -> convertExpr value
			_  -> convertedWithWhere
		whereVars = whereMap (\(Definition name _ _) -> name)
		whereValues = whereMap convertDef
		whereMap f = map f whereDefinitions
		convertedWithWhere = CL (convertExpr value) $ W $ zip whereVars whereValues
		value = letValue l
		whereDefinitions = letWhere l