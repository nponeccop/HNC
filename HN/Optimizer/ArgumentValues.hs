{-# LANGUAGE GADTs #-}
module HN.Optimizer.ArgumentValues where

import Compiler.Hoopl
import Data.Functor.Foldable
import HN.Optimizer.Lattice
import HN.Optimizer.Node

type ArgFact = WithTopAndBot [WithTopAndBot ExpressionFix]

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

argLattice :: Eq a => DataflowLattice (WithTopAndBot [WithTopAndBot a])
argLattice = listLattice singleArgLattice "Jo"

varArgs a = case a of
	Atom _ -> []
	Application (Fix (Atom var), _) bb -> let (xx, yy) = unzip bb in (var, xx) : concat yy
	Application aa bb -> concat $ snd $ unzip $ aa : bb
	_ -> []

process :: ExpressionFix -> [(Label, [ExpressionFix])]
process = para varArgs

fff (k, v) = (k, PElem $ map PElem v)

transferF = mkFTransfer ft where
	ft :: Node e x -> ArgFact -> Fact x ArgFact
	ft (Entry _) _ = undefined
	ft (Exit dn) _ = case dn of
		LetNode _ v -> mkFactBase argLattice $ map fff $ process v
		ArgNode -> mapEmpty
		LibNode -> mapEmpty

avPass :: FwdPass
          SimpleFuelMonad
          HN.Optimizer.Node.Node
          (WithTopAndBot [WithTopAndBot HN.Optimizer.Node.ExpressionFix])

avPass = FwdPass 
	{ fp_lattice = argLattice
	, fp_transfer = transferF
	, fp_rewrite = noFwdRewrite
	}
