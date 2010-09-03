module HN.TypeTools where
import SPL.Types
import qualified Data.Set as S
import Data.List
import Data.Maybe
---
--
isFunctionType (TT _) = True
isFunctionType _ = False

isPolymorphicFunctionType x = isFunctionType x && not (S.null $ typeTu x)

hasFunctionalType x = isJust $ find isFunctionType $ init x

cppCannotInferReturnType x = not $ S.null $ (typeTu $ last x) S.\\ (typeTu $ TT $ init x)


typeTu x = let union = S.unions . map typeTu in case x of
	TU v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty

typeAllPolyVars x = let union = S.unions . map typeAllPolyVars in case x of
	TU v -> S.singleton v
	TV v -> S.singleton v
	TT l -> union l
	TD _ l -> union l
	_    -> S.empty

typeTv x = let union = S.unions . map typeTv in case x of
		TV v -> S.singleton v
		TT l -> union l
		TD _ l -> union l
		_    -> S.empty

mapTypeTU f t = subst t where
	subst t = case t of
		TU a -> f (TU a)
		TV a -> f (TU a)
		TT a -> TT $ map subst a
  		TD a b -> TD a (map subst b)
		_ -> t
