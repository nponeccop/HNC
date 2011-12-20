module HN.TypeTools (isFunctionType, hasFunctionalType, cppCannotInferReturnType, typeTu, typeTv, mapTypeTV, removeTU, addTU, constantType, tv) where
import SPL.Types
import qualified Data.Set as S
import Data.List
import Data.Maybe
import HN.Intermediate
---
--
isFunctionType (TT _) = True
isFunctionType _ = False

hasFunctionalType x = isJust $ find isFunctionType $ init x

cppCannotInferReturnType x = not $ S.null $ typeTu (last x) S.\\ typeTu (TT $ init x)

tv x = TV $ 't' : show x

-- используется при выводе типа константы в качестве tau
constantType x = case x of
	ConstInt _ -> T "num"
	ConstString _ -> T "string"

typeTu x = let union = S.unions . map typeTu in case x of
	TU v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty

typeTv x = let union = S.unions . map typeTv in case x of
		TV v -> S.singleton v
		TT l -> union l
		TD _ l -> union l
		_    -> S.empty

removeTU x = case x of
	TU v -> TV v
	TT l -> TT $ map removeTU l
	TD x l -> TD x $ map removeTU l
	a    -> a

addTU s = mapTypeTV f where
	f x | x `S.member` s = TU x
  	f x = TV x


mapTypeTV f t = subst t where
	subst t = case t of
		TU a -> TU a
		TV a -> f a
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t
