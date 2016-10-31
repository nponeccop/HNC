module HN.SplExport (convertToSpl, convertExpr, convertDef) where
import HN.Intermediate
import SPL.Visualise (showAsSource)
import SPL.Types
import Utils (joinStr)

convertToSpl = (\x -> show x ++ "\n" ++ joinStr "\n" (map showAsSource x)) . map convertDef

convertExpr (Constant (ConstInt i)) = CNum i
convertExpr (Constant (ConstString i)) = CStr i

convertExpr (Atom a) = CVal a
convertExpr (Application a b) = CL (convertExpr a) (K $ map convertExpr b)
convertExpr expr = error $ show expr

convertDef (Definition _ arguments l)
	= (case arguments of
		[] -> convertedWithWhere
		_ -> CL xvalue (S arguments)) where
		xvalue = case whereDefinitions of
			[] -> convertExpr value
			_  -> convertedWithWhere
		whereVars = whereMap (\(Definition name _ _) -> name)
		whereValues = whereMap convertDef
		whereMap f = map f whereDefinitions
		convertedWithWhere = CL (convertExpr value) $ W $ zip whereVars whereValues
		value = letValue l
		whereDefinitions = letWhere l