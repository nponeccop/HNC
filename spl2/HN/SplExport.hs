module HN.SplExport where
import HN.Intermediate
import SPL.Types

convertExpr (Constant (ConstInt i)) = CNum i 0
convertExpr (Constant (ConstString i)) = CStr i 0

convertExpr (Atom a) = CVal a 0
convertExpr (Application a b) = CL (convertExpr a) (K $ map convertExpr b) 0
convertExpr expr = error $ show expr 

convertDef (Definition _ [] value []) = convertExpr value
-- convertDef def @ (Definition _ [] value _) = error $ show def

convertDef (Definition _ arguments value whereDefinitions) 
	= CL xvalue (S arguments) 0 where
		xvalue = case whereDefinitions of
			[] -> convertExpr value
			_  -> CL (CL (convertExpr value) (S whereVars) 0) (K whereValues) 0
		whereVars = whereMap (\(Definition name _ _ _) -> name)
		whereValues = whereMap convertDef
		whereMap f = map f whereDefinitions
