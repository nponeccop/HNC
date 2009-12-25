module MilnerRef where
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Control.Monad.State

data Type
	= Primitive String
	| Function Type Type
	| TypeVar Int deriving (Eq, Show)

data Term
	= Application Term Term
	| Atom String
	| Lambda String Term
	| TypedLambda String Int Term -- unused here
	deriving (Show)

data Unify = Unify Type Type deriving (Eq, Show)

data Subst = Subst Int Type deriving (Eq,Show)


contains _ _ = False

xreplace a t e = map (\(Unify x y) -> Unify (rr x) (rr y)) e where
	rr (TypeVar x) = if x == a then t else TypeVar x
	rr (Function x y) = Function (rr x) (rr y)
	rr x = x

nextVar = do
	x <- get
	put (x + 1)
	return $ TypeVar x

getGamma x = fromJust . M.lookup x

addGamma gamma x alpha = M.insert x alpha gamma

generate gamma (Atom x) sigma = return [Unify sigma $ getGamma x gamma]

generate gamma (Application m n) sigma = do
	alpha <- nextVar
	liftM2 (++) (generate gamma m $ Function alpha sigma) (generate gamma n alpha)

generate gamma (Lambda x m) sigma = do
	alpha <- nextVar
	beta <- nextVar
	aa <- generate (addGamma gamma x alpha) m beta
	return (Unify sigma (Function alpha beta) : aa )

solve [] = []

solve ((Unify (Primitive b1) (Primitive b2)) : e)
	= if b1 == b2 then solve e else error "Type error 1"

solve ((Unify (Primitive _) (Function _ _)) : _)= error "Type error 2"

solve ((Unify (TypeVar a) t) : e)
	| t == TypeVar a = solve e
 	| contains t a = error "Type error 3"
	| otherwise = Subst a t : solve (xreplace a t e)

solve (Unify (Function a1 b1) (Function a2 b2) : e) = solve $ Unify a1 a2 : Unify b1 b2 : e

solve ((Unify a b) : e) = solve $ Unify b a : e

getMain u = (case (fromJust $ find f u) of Subst _ x -> x) where
	f (Subst 0 _) = True
	f _ = False

runGen term = fst $ runState f 1 where
	f = generate M.empty term $ TypeVar 0
