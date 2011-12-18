module HN.Optimizer.WithGraph where
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler

withGraph f libNames def = decompileGraph labelNames $ f graph where
	(graph, (_, labelNames)) = compileGraph libNames def

