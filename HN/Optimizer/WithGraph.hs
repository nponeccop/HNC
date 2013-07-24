module HN.Optimizer.WithGraph where
import HN.Optimizer.GraphCompiler
import HN.Optimizer.GraphDecompiler
import Utils

withGraph libNames f def = decompileGraph (xtrace "labelNames" labelNames) $ f graph where
	(graph, (_, labelNames)) = compileGraph libNames def
