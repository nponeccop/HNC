{-# LANGUAGE GADTs #-}
module HN.Optimizer.ArgumentValues where

import Compiler.Hoopl
import Data.Functor.Foldable
import qualified Data.Map as M
import HN.Optimizer.Lattice
import HN.Optimizer.Node

type ArgFact = WithTopAndBot [WithTopAndBot ExpressionFix]

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

argLattice :: Eq a => DataflowLattice (WithTopAndBot [WithTopAndBot a])
argLattice = listLattice singleArgLattice "Jo"

joinA a b = fact_join singleArgLattice undefined (OldFact a) (NewFact b) 

varArgs a = case a of
	Atom _ -> M.empty
	Application (Fix (Atom var), _) bb -> let (xx, yy) = unzip bb in M.insertWith (++) var [xx] $ M.unions yy
	Application _ _ -> M.empty
	_ -> M.empty
	--(_, aa)  (_, bb) -> let (xx, yy) = unzip bb in M.unions yy

process :: ExpressionFix -> M.Map Label [[ExpressionFix]]
process = para varArgs

mapFromDataMap a = mapFromList $ M.toList a

fff (k, v) = map (\x -> (k, PElem $ map PElem x)) v

transferF = mkFTransfer ft where
	ft :: Node e x -> ArgFact -> Fact x ArgFact
	ft (Entry _) _ = undefined
	ft (Exit dn) _ = case dn of
		LetNode _ v -> mkFactBase argLattice $ concatMap fff $ M.toList $ process v
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
