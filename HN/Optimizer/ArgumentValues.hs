{-# LANGUAGE GADTs #-}
module HN.Optimizer.ArgumentValues where

import Compiler.Hoopl
import Control.Arrow
import qualified Data.Foldable as F
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
	Application (Fix (Atom var)) xx -> [(var, xx)]
	_ -> []

unzippedPara f = para $ \a -> f (fmap fst a) (fmap snd a)

process :: ExpressionFix -> [(Label, [ExpressionFix])]
process = unzippedPara $ \f s -> F.concat s ++ varArgs f

transferF = mkFTransfer ft where
	ft :: Node e x -> ArgFact -> Fact x ArgFact
	ft (Entry _) _ = undefined
	ft (Exit dn) _ = mkFactBase argLattice $ map (second $ PElem . map PElem) $ case dn of
		LetNode _ v -> process v
		ArgNode -> []
		LibNode -> []

avPass :: FwdPass
          SimpleFuelMonad
          HN.Optimizer.Node.Node
          (WithTopAndBot [WithTopAndBot HN.Optimizer.Node.ExpressionFix])

avPass = FwdPass 
	{ fp_lattice = argLattice
	, fp_transfer = transferF
	, fp_rewrite = noFwdRewrite
	}
