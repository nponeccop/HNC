{-# LANGUAGE GADTs, StandaloneDeriving #-}
module HN.Optimizer.ArgumentValues where

import Compiler.Hoopl
import Control.Arrow
import qualified Data.Foldable as F
import Data.Functor.Foldable hiding (Fix)

import HN.Intermediate
import HN.Optimizer.Lattice
import HN.Optimizer.Node
import HN.Optimizer.Pass

type ArgFact = WithTopAndBot [WithTopAndBot ExpressionFix]

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

deriving instance Show ChangeFlag

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

argLattice :: Eq a => DataflowLattice (WithTopAndBot [WithTopAndBot a])
argLattice = listLattice (fact_join singleArgLattice) "Jo"

varArgs a = case a of
	ApplicationF (Atom var) xx -> [(var, xx)]
	_ -> []

unzippedPara f = para $ \a -> f (fmap fst a) (fmap snd a)

process :: ExpressionFix -> [(Label, [ExpressionFix])]
process = unzippedPara $ \f s -> F.concat s ++ varArgs f

transferF = mkFTransfer ft where
	ft :: Node e x -> ArgFact -> Fact x ArgFact
	ft (Entry _) f = f
	ft (Exit dn) f = mkFactBase argLattice $ map (second $ PElem . map PElem) $ case dn of
		LetNode args value -> unzipArgs f args ++ process value
		ArgNode -> []
		LibNode -> []

unzipArgs :: ArgFact -> [Label] -> [(Label, [ExpressionFix])]
unzipArgs (PElem actualArgs) formalArgs
	| length actualArgs /= length formalArgs = error "Wrong formalArgs"
	| otherwise = concatMap foo $ zip formalArgs actualArgs
unzipArgs _ _ = []

foo (formalArg, PElem actualArg) = [(formalArg, [actualArg])]
foo _ = []

avPass :: FwdPass SimpleFuelMonad Node ArgFact
avPass = FwdPass 
	{ fp_lattice = argLattice
	, fp_transfer = transferF
	, fp_rewrite = noFwdRewrite
	}

runAv = runPass (analyzeAndRewriteFwd avPass) (\_ _ -> mapEmpty)
