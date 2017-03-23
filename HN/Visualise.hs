{-# LANGUAGE LambdaCase #-}

module HN.Visualise (formatHN, showE) where

import HN.Intermediate
import Utils

formatHN = joinStr "\n" . map showD

showD x = joinStr "\n" $ showDD x

showDD (Definition name args letIn) = case letWhere letIn of
	[] -> [ name ++ concatMap (' ' :) args ++ " = " ++ showE (letValue letIn) ]
	w -> [ name ++ concatMap (' ' :) args ++ " = {"] ++ showW w ++ ["\t" ++ showE (letValue letIn), "}"]

showW l = map ("\t" ++) $ concatMap showDD l

showE = \case
	Atom a -> a
	Constant a -> show a
	Application (Atom a) b -> a ++ concatMap (\x -> " " ++ showFunctionArg x) b
	Application a b -> inParens (showE a) ++ concatMap (\x -> " " ++ showFunctionArg x) b

showFunctionArg = \case
	Atom a -> a
	Constant a -> show a
	e -> inParens (showE e)

