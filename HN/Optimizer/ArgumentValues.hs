{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, DeriveFoldable #-}
module HN.Optimizer.ArgumentValues (runAv, ArgFact, argLattice, AFType) where

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

type AFType = (WithTopAndBot [WithTopAndBot ExpressionFix], WithTopAndBot ExpressionFix)

type ArgFact = (AFType, M.Map Label AFType)

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

deriving instance Show ChangeFlag
deriving instance Foldable (Prim [a])

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

argLattice :: DataflowLattice ArgFact
argLattice = dataflowLattice

instance Lattice (WithTopAndBot [WithTopAndBot ExpressionFix]) where
	dataflowLattice = addPoints' "AV.Call" $ joinLists (fact_join singleArgLattice)

instance Lattice (WithTopAndBot ExpressionFix) where
	dataflowLattice = flatEqLattice "AV.Value"

varArgs a = case a of
	ApplicationF (Atom var) xx -> [(var, xx)]
	_ -> []

unzippedPara f = para $ \a -> f (fmap fst a) (fmap snd a)

process2 :: ExpressionFix -> [(Label, [ExpressionFix])]
process2 = unzippedPara $ \f s -> F.concat s ++ varArgs f

transferF :: Node e x -> ArgFact -> Fact x ArgFact
transferF (Entry l) (_, m) = (fromMaybe bot $ M.lookup l m, m)

transferF n @ (Exit (LetNode args value)) ((callFact, _), _)
	= distributeFact n $ (,) bot $ M.fromList $ (map (second $ (\x -> (x, bot)) . PElem . map PElem) $ process2 value) ++ (map (second $ (,) bot . PElem) $ unzipArgs callFact args)

transferF (Exit _) _ = noFacts

unzipArgs :: WithTopAndBot [WithTopAndBot ExpressionFix] -> [Label] -> [(Label, ExpressionFix)]
unzipArgs (PElem actualArgs) formalArgs = xtrace "xaaa" $ concatMap foo $ zipExactNote "unzipArgs" formalArgs actualArgs
unzipArgs Bot _ = xtrace "bot" []
unzipArgs Top _ = error "top!"

foo (formalArg, PElem actualArg) = [(formalArg, actualArg)]
foo _ = []

avPass :: FwdPass SimpleFuelMonad Node ArgFact
avPass = FwdPass 
	{ fp_lattice = argLattice
	, fp_transfer = mkFTransfer transferF
	, fp_rewrite = noFwdRewrite
	}

runAv :: Pass any ArgFact
runAv = runPass (analyzeAndRewriteFwd avPass) (\_ _ -> mapEmpty)

