{-# LANGUAGE LambdaCase, GADTs, StandaloneDeriving, FlexibleInstances, DeriveFoldable,  MultiParamTypeClasses, FlexibleContexts #-}
module HN.Optimizer.ArgumentValues (runAv, ArgFact, AFType) where

import Compiler.Hoopl
import qualified Data.Map as M
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Lattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.Utils
import Utils.Kmett (unzippedPara, mapValues)

type AFType = (WithTopAndBot [WithTopAndBot ExpressionFix], WithTopAndBot ExpressionFix)

type ArgFact = (AFType, M.Map Label AFType)

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

deriving instance Show ChangeFlag

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

instance Lattice (WithTopAndBot [WithTopAndBot ExpressionFix]) where
	dataflowLattice = addPoints' "AV.Call" $ joinLists (fact_join singleArgLattice)

instance Lattice (WithTopAndBot ExpressionFix) where
	dataflowLattice = flatEqLattice "AV.Value"

varArgs = \case ApplicationF (Atom var) xx -> [(var, xx)]; _ -> []

process2 :: ExpressionFix -> [(Label, [ExpressionFix])]
process2 = unzippedPara $ \f s -> concat s ++ varArgs f

onlyCall x = (PElem $ map PElem x, bot)
onlyValue x = (bot,  x)

transferF :: DefinitionNode -> AFType -> [(Label, AFType)]
transferF (LetNode args value) (callFact, _)
	= mapValues onlyCall (process2 value) ++ mapValues onlyValue (unzipArgs callFact args)

transferF _ _ = []

unzipArgs :: WithTopAndBot [WithTopAndBot ExpressionFix] -> [Label] -> [(Label, WithTopAndBot ExpressionFix)]
unzipArgs (PElem actualArgs) formalArgs = zipExactNote "unzipArgs" formalArgs actualArgs
unzipArgs Bot _ = []
unzipArgs Top _ = []

runAv :: Pass ArgFact ArgFact
runAv = runPassF PassParams
	{ ppConvertFacts = const
	, ppTransfer = mkFTransfer $ transferMapExitF transferF
	, ppRewrite = noFwdRewrite
	}
