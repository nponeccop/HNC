{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, DeriveFoldable #-}
module HN.Optimizer.ArgumentValues (runAv, ArgFact) where

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

type ArgFact = WithTopAndBot [WithTopAndBot ExpressionFix]

type SingleArgLattice a = DataflowLattice (WithTopAndBot a)

deriving instance Show ChangeFlag
deriving instance Foldable (Prim [a])

singleArgLattice :: Eq a => SingleArgLattice a
singleArgLattice = flatEqLattice "ArgumentValues"

argLattice :: Eq a => DataflowLattice (WithTopAndBot [WithTopAndBot a])
argLattice = listLattice (fact_join singleArgLattice) "Jo"

varArgs a = case a of
	ApplicationF (Atom var) xx -> [(var, xx)]
	_ -> []

unzippedPara f = para $ \a -> f (fmap fst a) (fmap snd a)

process2 :: ExpressionFix -> [(Label, [ExpressionFix])]
process2 = unzippedPara $ \f s -> F.concat s ++ varArgs f



ft :: Node e x -> ArgFact -> Fact x ArgFact
ft (Entry _) f = f
ft n @ (Exit dn) f = mkFactBase argLattice $ (++) (defaultFactsHack argLattice n) $ map (second $ PElem . map PElem) $ case dn of
	LetNode args value -> unzipArgs f args ++ process2 value
	ArgNode -> []
	LibNode -> []

unzipArgs :: ArgFact -> [Label] -> [(Label, [ExpressionFix])]
unzipArgs (PElem actualArgs) formalArgs = concatMap foo $ zipExactNote "Wrong formalArgs" formalArgs actualArgs
unzipArgs _ _ = []

foo (formalArg, PElem actualArg) = [(formalArg, [actualArg])]
foo _ = []

no _ _ = Nothing

cp :: Node e x -> ArgFact -> Maybe (Graph Node e x)
cp (Entry _) _ = Nothing
cp (Exit ArgNode) (PElem [PElem x]) = Just $ mkLast $ Exit $ LetNode [] x
cp (Exit (LetNode l x)) (PElem f) = (\l -> mkLast $ Exit $ LetNode l x) <$> rewriteFormalArgs f l
cp (Exit _) _ = Nothing

rewriteFormalArgs :: [WithTopAndBot ExpressionFix] -> Rewrite [Label] 
rewriteFormalArgs actualArgs formalArgs
	= map fst <$> process foo (zipExactNote "Wrong formalArgs during rewrite" formalArgs actualArgs)
	where
		foo ((_, PElem _) : tail) = Just tail
		foo _ = Nothing

avPass :: FwdPass SimpleFuelMonad Node ArgFact
avPass = FwdPass 
	{ fp_lattice = argLattice
	, fp_transfer = mkFTransfer ft
	, fp_rewrite = pureFRewrite no
	}

runAv = runPass (analyzeAndRewriteFwd avPass) (\_ _ -> mapEmpty)
