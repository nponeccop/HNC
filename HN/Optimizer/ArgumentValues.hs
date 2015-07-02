{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, DeriveFoldable #-}
module HN.Optimizer.ArgumentValues (runAv, ArgFact, argLattice) where

import Compiler.Hoopl
import Control.Arrow
import qualified Data.Foldable as F
import Data.Functor.Foldable hiding (Fix, Foldable)
import Safe.Exact

import HN.Intermediate
import HN.Optimizer.Lattice
import HN.Optimizer.Node
import HN.Optimizer.Pass
import HN.Optimizer.ExpressionRewriter
import HN.Optimizer.Utils
import Utils

import HN.Optimizer.Visualise ()

data AFType = Call [WithTopAndBot ExpressionFix] | Value ExpressionFix deriving (Eq, Show)

type ArgFact = WithTopAndBot AFType

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

deriving instance Show ChangeFlag
deriving instance Foldable (Prim [a])

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

--argLattice :: Eq a => DataflowLattice (WithTopAndBot [WithTopAndBot a])
--argLattice = listLattice (fact_join singleArgLattice) "Jo"

argLattice :: DataflowLattice ArgFact
argLattice = liftedLattice f "ArgFact" where
	f (OldFact o @ (Value _)) (NewFact n) = if o == n then Bot else Top
	f (OldFact o @ (Call ol)) (NewFact n @ (Call on)) = case joinLists (fact_join singleArgLattice) undefined (OldFact ol) (NewFact on) of
		(NoChange, _) -> Bot
		(_, Top) -> Top
		(_, PElem j) -> PElem $ Call j

varArgs a = case a of
	ApplicationF (Atom var) xx -> [(var, xx)]
	_ -> []

unzippedPara f = para $ \a -> f (fmap fst a) (fmap snd a)

process2 :: ExpressionFix -> [(Label, [ExpressionFix])]
process2 = unzippedPara $ \f s -> F.concat s ++ varArgs f

transferF :: DefinitionNode -> ArgFact -> FactBase ArgFact
transferF dn @ (LetNode args value) f @ (PElem (Call argValues)) = mkFactBase argLattice
	$ (map (second $ PElem . Call . map PElem) $ process2 value) ++ (defaultFactsHack argLattice (Exit dn)) ++ (map (second $ PElem . Value . head) $ unzipArgs f args)

transferF dn @ (LetNode args value) _ = mkFactBase argLattice
	$ defaultFactsHack argLattice (Exit dn)
		++ (map (second $ PElem . Call . map PElem) $ process2 value)
transferF _ _ = noFacts

unzipArgs :: ArgFact -> [Label] -> [(Label, [ExpressionFix])]
unzipArgs (PElem (Call actualArgs)) formalArgs = ztrace "xaaa" $ concatMap foo $ zipExactDef [] formalArgs actualArgs
unzipArgs (PElem (Value _)) [] = []
unzipArgs Bot _ = ztrace "bot" []
unzipArgs Top _ = error "top!"

foo (formalArg, PElem actualArg) = [(formalArg, [actualArg])]
foo _ = []

no _ _ = Nothing

cp :: DefinitionNode -> ArgFact -> Maybe DefinitionNode
cp ArgNode (PElem (Value x)) = ztrace "newArg" $ Just $ LetNode [] x
cp ArgNode (PElem e) = error $ show $ "aaa = " ++ show e
cp ArgNode Bot = Nothing
cp ArgNode _ = error "ooo"
cp (LetNode [] _) _ = Nothing
cp (LetNode l x) (PElem f) = Nothing -- (\l -> LetNode l x) <$> rewriteFormalArgs f l
cp _ _ = Nothing

rewriteFormalArgs :: [WithTopAndBot ExpressionFix] -> Rewrite [Label] 
rewriteFormalArgs actualArgs formalArgs
	= map fst <$> process foo (zipExactNote "Wrong formalArgs during rewrite" formalArgs actualArgs)
	where
		foo ((_, PElem _) : tail) = Just tail
		foo _ = Nothing

avPass :: FwdPass SimpleFuelMonad Node ArgFact
avPass = FwdPass 
	{ fp_lattice = argLattice
	, fp_transfer = mkFTransfer $ transferExitF transferF
	, fp_rewrite = pureFRewrite $ rewriteExitF cp
	}

runAv :: Pass any ArgFact
runAv = runPass (analyzeAndRewriteFwd avPass) (\_ _ -> mapEmpty)
