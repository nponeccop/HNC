module HN.Visualise (showD) where

import HN.Intermediate
import Utils

showD x = joinStr "\n" $ showDD x

showDD (Definition name args letIn) = case letWhere letIn of
	[] -> [ name ++ concatMap (' ' :) args ++ " = " ++ showE (letValue letIn) ]
	w -> [ name ++ concatMap (' ' :) args ++ " = {"] ++ showW w ++ ["\t" ++ showE (letValue letIn), "}"]

showW l = map ("\t" ++) $ concatMap showDD l

showE e = case e of
	Atom a -> a
	Constant a -> show a
	Application (Atom a) b -> a ++ concatMap (\x -> " " ++ showFunctionArg x) b
	Application a b -> inParens (showE a) ++ concatMap (\x -> " " ++ showFunctionArg x) b


showFunctionArg e = case e of
	Atom a -> a
	Constant a -> show a
	_ -> inParens (showE e)

