{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, DeriveFoldable #-}
module HN.Optimizer.ArgumentValues (runAv, ArgFact, argLattice, AFType(..)) where

import Compiler.Hoopl
import Control.Arrow
import qualified Data.Foldable as F
import Data.Functor.Foldable hiding (Fix, Foldable)
import qualified Data.Map as M
import Data.Maybe
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.ClassyLattice
import HN.Optimizer.Lattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import Utils

import HN.Optimizer.Visualise ()

data AFType = Call [WithTopAndBot ExpressionFix] | Value ExpressionFix deriving (Eq, Show)

type ArgFact = (WithTopAndBot AFType, M.Map Label (WithTopAndBot AFType))

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

deriving instance Show ChangeFlag
deriving instance Foldable (Prim [a])

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

argLattice :: DataflowLattice ArgFact
argLattice = dataflowLattice

instance Lattice (WithTopAndBot AFType) where
	dataflowLattice = liftedLattice f "ArgFact.AFType" where
			f (OldFact o @ (Value _)) (NewFact n) = if o == n then Bot else Top
			f (OldFact (Call ol)) (NewFact (Call on)) = case joinLists (fact_join singleArgLattice) undefined (OldFact ol) (NewFact on) of
				(NoChange, _) -> Bot
				(_, Top) -> Top
				(_, PElem j) -> PElem $ Call j
			f (OldFact o @ (Call _)) _ = Bot

varArgs a = case a of
	ApplicationF (Atom var) xx -> [(var, xx)]
	_ -> []

unzippedPara f = para $ \a -> f (fmap fst a) (fmap snd a)

process2 :: ExpressionFix -> [(Label, [ExpressionFix])]
process2 = unzippedPara $ \f s -> F.concat s ++ varArgs f

transferF :: Node e x -> ArgFact -> Fact x ArgFact
transferF (Entry l) (_, m) = (fromMaybe Bot $ M.lookup l m, m)
transferF n @ (Exit (LetNode args value)) (f @ (PElem (Call _)), _)
	= distributeFact n $ (,) Bot $ M.fromList $ (map (second $ PElem . Call . map PElem) $ process2 value) ++ (map (second $ PElem . Value . head) $ unzipArgs f args)

transferF n @ (Exit (LetNode _ value)) _ = distributeFact n $ (,) Bot $ M.fromList (map (second $ PElem . Call . map PElem) $ process2 value)
transferF (Exit _) _ = noFacts

unzipArgs :: WithTopAndBot AFType -> [Label] -> [(Label, [ExpressionFix])]
unzipArgs (PElem (Call actualArgs)) formalArgs = xtrace "xaaa" $ concatMap foo $ zipExactDef [] formalArgs actualArgs
unzipArgs (PElem (Value _)) [] = []
unzipArgs Bot _ = ztrace "bot" []
unzipArgs Top _ = error "top!"

foo (formalArg, PElem actualArg) = [(formalArg, [actualArg])]
foo _ = []

avPass :: FwdPass SimpleFuelMonad Node ArgFact
avPass = FwdPass 
	{ fp_lattice = argLattice
	, fp_transfer = mkFTransfer transferF
	, fp_rewrite = noFwdRewrite
	}

runAv :: Pass any ArgFact
runAv = runPass (analyzeAndRewriteFwd avPass) (\_ _ -> mapEmpty)

