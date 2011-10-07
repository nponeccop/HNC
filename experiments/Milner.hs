

-- UUAGC 0.9.23 (Milner.ag)

{-# LINE 76 "Milner.ag" #-}


data Unify a b = UnifyApplication a b

constantType _ = undefined
lookupAtom _ _ = undefined
addAtoms _ _ = undefined
replaceAtoms _ _ = undefined
argAtoms _ = undefined


merge a b = trace (show a ++ " ~ " ++ show b ++ " = " ++ show c) c where
	c = xmerge a b
	xmerge (TT a) (TT b)|length a == length b = TT $ zipWith xmerge a b
	xmerge (TD n a) (TD n2 b)|n==n2 && length a==length b = TD n $ zipWith xmerge a b
	xmerge (TU a) (TU b) = TU a
	xmerge (TU a) b = observeN ("xmerge1"++show a) b
	xmerge a (TU b) = observeN ("xmerge2"++show b) a
	xmerge (TV a) (TV b) = TV a
	xmerge (TV a) b = b
	xmerge a (TV b) = a
	xmerge TL TL = TL
	xmerge (T n) (T n2)|n==n2 = T n
	xmerge (TUL a) b = TUL (b:a)
	xmerge a b = TUL [b, a]
{-# LINE 32 "Milner.hs" #-}
-- Definition --------------------------------------------------
-- Expression --------------------------------------------------
-- ExpressionList ----------------------------------------------
-- Program -----------------------------------------------------