module SPL.Visualise where

import SPL.Types
import Utils

showAsSource (CL l (K x)) = showAsSource l ++ " " ++ joinStr " " (map (paren isComposed) x)

showAsSource (CL l (S x)) = joinStr "*" x ++  "*" ++ showAsSource l

showAsSource (CL l (W x)) = (paren isWhere l) ++ "*" ++ joinStr "*" (map showWhere x) where
	showWhere (x, y) = x ++ ":" ++ paren (\x -> isLambda x || isWhere x) y

showAsSource (CVal x) = x

showAsSource (CNum x) = show x

showAsSource (CBool True) = "1b"

showAsSource (CBool False) = "0b"

showAsSource (CStr x) = show x

showAsSource x = ">>>>>>>>>>" ++ show x ++ "<<<<<<<<<<<<<"

paren f y = if f y then "(" ++ yy ++ ")" else yy where
	yy = showAsSource y

isLambda (CL l (S x)) = True
isLambda _ = False

isApp (CL l (K x)) = True
isApp _ = False

isWhere (CL _ (W _)) = True
isWhere _ = False

isComposed (CL _ _) = True
isComposed _  = False

makeType (T x) = x
makeType (TD a b) =  a ++ " " ++ (joinStr " " $ map makeType b)
makeType (TT x) = joinStr " -> " $ map makeType2 x
makeType (TU x) = '?' : x
makeType x = show x

makeType2 (x @ (TT _)) = "(" ++ makeType x ++ ")"
makeType2 x = makeType x

printFF a (CTyped x y) = (if a then "[" ++ makeType x ++ "] " else "" ) ++  printFF a y
printFF a (CL x (K y)) = "(" ++ printFF a x ++ " " ++ (joinStr " " $ map (printFF a) y) ++ ")"
printFF a (CL x (S y)) =  "(\\" ++ concatMap (++ " ") y ++ "-> " ++ printFF2 a x ++ ")"
printFF _ (CVal x) = x
printFF _ (CNum x) = show x
printFF _ x = show x

printFF2 a (CL x (K y)) = printFF a x ++ " " ++ (joinStr " " $ map (printFF a) y)
printFF2 a (CTyped x y) = if a then "[" ++ makeType x ++ "] " ++ printFF a y else printFF2 a y
printFF2 a b = printFF a b

showTypedTree x = printFF True x
showTypedTreeWithoutTypes x = printFF False x
