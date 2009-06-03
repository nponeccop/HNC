module Main where
import Test.HUnit
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.List

data Type 
	= Primitive String 
	| Function Type Type
	| TypeVar Int deriving (Eq, Show)
	
data Term 
	= Application Term Term
	| Atom String
	| Lambda String Term
	| TypedLambda String Int Term
	deriving (Show)
	
data Unify = Unify Type Type deriving (Eq, Show)

data Subst = Subst Int Type deriving (Eq,Show)     

nextVar = do 
	x <- get
	put (x + 1)
	return $ TypeVar x
	
getGamma x gamma = fromJust $ M.lookup x gamma 

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

runGen term = fst $ runState f 1 where
	f = generate M.empty term $ TypeVar 0 	
   

solve [] = []

solve ((Unify (Primitive b1) (Primitive b2)) : e) 
	= if b1 == b2 then solve e else error "Type error 1"      

solve ((Unify (Primitive _) (Function _ _)) : _)= error "Type error 2"

solve ((Unify (TypeVar a) t) : e) = if t == TypeVar a 
	then solve e
	else
		if contains t a 
			then error "Type error 3"
			else (Subst a t) : (solve $ xreplace a t e)
			
solve (Unify (Function a1 b1) (Function a2 b2) : e) = solve $ (Unify a1 a2) : (Unify b1 b2) : e

solve ((Unify a b) : e) = solve $ (Unify b a) : e






contains _ _ = False

xreplace a t e = map (\(Unify x y) -> Unify (rr x) (rr y)) e where
	rr (TypeVar x) = if x == a then t else TypeVar x
	rr (Function x y) = Function (rr x) (rr y)
	rr x = x
	
getMain u = (case (fromJust $ find f u) of Subst _ x -> x) where
	f (Subst 0 _) = True
	f _ = False		     

tt x y = TestCase $ assertEqual "foo" x y

typeCheck term = getMain $ solve $ reverse $ runGen $ term

x0 = tt [] (solve [Unify (TypeVar 0) (TypeVar 0)])
x1 = tt [Subst 0 $ TypeVar 1] (solve [Unify (TypeVar 0) (TypeVar 1)])
x2 = tt [Subst 0 $ Primitive "a"] (solve [Unify (TypeVar 0) (Primitive "a")])
x3 = tt [Subst 0 $ Primitive "a", Subst 1 $ Primitive "a"] $ solve [Unify (TypeVar 0) (Primitive "a"), Unify (TypeVar 1) (TypeVar 0)]
x4 = tt [Unify (TypeVar 0) (Function (TypeVar 1) (TypeVar 2)),Unify (TypeVar 2) (TypeVar 1)] $ runGen (Lambda "x" $ Atom "x")
x5 = tt (Function (TypeVar 1) (TypeVar 1)) $ typeCheck (Lambda "x" $ Atom "x")

runTests = runTestTT $ TestList [x0, x1, x2, x3, x4, x5]

main = do
	print $ runGen $ (Lambda "x" $ Atom "x")
	print $ solve $ reverse $ runGen $ (Lambda "x" $ Atom "x")
	print $ fst $ runState (makeTree (Lambda "x" $ Atom "x")) 1
	runTests
	 
makeTree (Application m n) = do
	nextVar
	aa <- (makeTree m) 
	bb <- (makeTree n)
	return $ Application aa bb  

makeTree t @ (Lambda x m) = do 
	alpha <- nextVar
	beta <- nextVar
	xx <- makeTree m 
	return $ TypedLambda x (fromTv alpha) t
	
makeTree t = return t 
 
fromTv x = case x of TypeVar a -> a 
