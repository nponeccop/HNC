
import CPP.CompileTools
import HN.Optimizer.ArgumentValues
import HN.Optimizer.GraphCompiler
import FFI.TypeParser
import HN.Optimizer.Node
import Compiler.Hoopl

main = do
	ast <- parseHN "hn_tests/print15.hn"
	ffi <- importHni "lib/lib.hni"
	print $ run avPass $ fst $ compileGraph ffi $ head ast

run pass graph = case runSimpleUniqueMonad . runWithFuel infiniteFuel $ analyzeAndRewriteFwd pass entry graph mapEmpty of
	(_, newFacts, _) -> newFacts
	
	
	
entry = JustC [runSimpleUniqueMonad freshLabel]
