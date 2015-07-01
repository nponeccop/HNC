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
import Utils;

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

ft :: DefinitionNode -> ArgFact -> FactBase ArgFact
ft dn f = ztrace "fb" $ mkFactBase argLattice $ (++) (defaultFactsHack argLattice (Exit dn)) $ map (second $ PElem . map PElem) $ case dn of
	LetNode args value -> unzipArgs f (ztrace "args" args) ++ process2 value
	ArgNode -> []
	LibNode -> []

unzipArgs :: ArgFact -> [Label] -> [(Label, [ExpressionFix])]
unzipArgs (PElem actualArgs) formalArgs = ztrace "xaaa" $ concatMap foo $ zipExactDef [] formalArgs actualArgs
unzipArgs Bot _ = ztrace "bot" []
unzipArgs Top _ = error "top!"

foo (formalArg, PElem actualArg) = [(formalArg, [actualArg])]
foo _ = []

no _ _ = Nothing

cp :: DefinitionNode -> ArgFact -> Maybe DefinitionNode
cp ArgNode (PElem [PElem x]) = Just $ LetNode [] $ ztrace "newArg" x
cp ArgNode (PElem e) = error $ show $ "aaa = " ++ show e
cp ArgNode Bot = Nothing
cp ArgNode _ = error "ooo"
cp (LetNode l x) (PElem f) = (\l -> LetNode l x) <$> rewriteFormalArgs f l
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
	, fp_transfer = mkFTransfer $ transferExitF ft
	, fp_rewrite = pureFRewrite $ rewriteExit cp
	}

runAv :: Pass any ArgFact
runAv = runPass (analyzeAndRewriteFwd avPass) (\_ _ -> mapEmpty)
