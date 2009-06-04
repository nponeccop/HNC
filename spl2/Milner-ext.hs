module Main where
import MilnerRef
import Test.HUnit
import Control.Monad.State
   
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

