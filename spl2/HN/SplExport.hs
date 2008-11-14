module HN.SplExport where
import Intermediate
import Types

convertExpr (Constant (ConstInt i)) = CNum i
convertExpr (Constant (ConstString i)) = CStr i

convertExpr (Atom a) = CVal a
convertExpr (Application a b) = CL (convertExpr a) $ K $ map convertExpr b
convertExpr expr = error $ show expr 

convertDef (Definition _ [] value []) = convertExpr value
-- convertDef def @ (Definition _ [] value _) = error $ show def

convertDef (Definition _ arguments value whereDefinitions) 
	= CL xvalue $ S arguments where
		xvalue = case whereDefinitions of
			[] -> convertExpr value
			_  -> CL (CL (convertExpr value) $ S whereVars) $ K whereValues
		whereVars = whereMap (\(Definition name _ _ _) -> name)
		whereValues = whereMap convertDef
		whereMap f = map f whereDefinitions
