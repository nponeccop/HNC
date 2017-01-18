module Unifier.Restricted
	( Unifier.Restricted.unify
	, Unifier.Restricted.subsumes
	, runErrorT2
	, WithEnv
	) where

import Unifier.Unifier

import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Control.Monad.Except

type Failure = UFailure T IntVar
type Type = UTerm T IntVar
type WithEnv a = IntBindingT T a
type WithExcept = ExceptT Failure

unify :: Monad a => Type -> Type -> WithExcept (WithEnv a) Type
unify = Control.Unification.unify

subsumes :: Monad a => Type -> Type -> WithExcept (WithEnv a) Bool
subsumes = Control.Unification.subsumes

runErrorT2 :: WithExcept m a -> m (Either Failure a)
runErrorT2 = runExceptT

