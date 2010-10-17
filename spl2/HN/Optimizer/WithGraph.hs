module HN.Optimizer.WithGraph where
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler

withGraph f libNames def = decompileGraph2 labelNames $ f $  graph where
	(graph, (_, labelNames)) = compileGraph2 libNames def



