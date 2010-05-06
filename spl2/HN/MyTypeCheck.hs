module HN.MyTypeCheck where
import qualified Data.Map as M
import HN.Intermediate
import Control.Monad.State

data Type = Fun [Type] | Type String | TypeVar Int | ADT [Type] deriving (Eq, Show)

data Env = Env Int (M.Map String Type) deriving (Show)



newTypeVar (Env maxId env) val = (Env newId $ M.insert val ntv env, ntv) where
	newId = maxId + 1
	ntv = TypeVar newId
	
envLookup (Env _ env) a = M.lookup a env   

typeCheck e (Atom a) = case envLookup e a of
	Just val -> (e, val)
	Nothing -> newTypeVar e a

{-

typeCheck e (Application a b) = (argEnv, funType)
 	where
 		(funEnv, funType)  = typeCheck e a 
		(argEnv, argTypes) = foldl argFolder (funEnv, []) b
		x = zipWith (argZipper) (funArgTypes funType) argTypes
		
argZipper ft at e = unify ft ft e

funArgTypes (Fun t) = init t

argFolder (e, l) a = (e', t : l) where
	(e', t) = typeCheck e a
	
ue t1 t2 = error "Cannot unify " ++ show t1 ++ " and " ++ show t2
	
unify t1 @ (Type _) t2 @ (Type _) e = if t1 == t2 then (t1, e) else (t1, e)
unify t1 @ (Type _) t2 e = (ue t1 t2

unify t1 t2 @ (TypeVar _) e = replaceVar t2 t1 e
unify t1 @ (TypeVar _) t2 e = replaceVar t1 t2 e

unify t1 t2 e = error "Not implemented - cannot unify " ++ show t1 ++ " and " ++ show t2

replaceVar t2 t1 e = (t1, e') where
	e' = e 

-}
