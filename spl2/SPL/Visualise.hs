module SPL.Visualise where

import SPL.Types
import Utils

showAsSource (CL l (K x)) = showAsSource l ++ " " ++ joinStr " " (map (paren isComposed) x)

showAsSource (CL l (S x)) = joinStr "*" x ++  "*" ++ showAsSource l

showAsSource (CL l (W x)) = (paren (const False) l) ++ "*" ++ joinStr "*" (map showWhere x) where
	showWhere (x, y) = x ++ ":" ++ paren isLambda y

showAsSource (CVal x) = x

showAsSource (CNum x) = show x

showAsSource x = ">>>>>>>>>>" ++ show x ++ "<<<<<<<<<<<<<"

paren f y = if f y then "(" ++ yy ++ ")" else yy where
	yy = showAsSource y

isLambda (CL l (S x)) = True
isLambda _ = False

isApp (CL l (K x)) = True
isApp _ = False

isComposed (CL _ _) = True 
isComposed _  = False
