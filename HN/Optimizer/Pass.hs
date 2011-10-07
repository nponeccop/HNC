module HN.Optimizer.Pass (runPass) where
import Compiler.Hoopl


runPass f makeFacts (graph, facts, _) = f (JustC [firstLabel]) graph $ makeFacts facts firstLabel where
	firstLabel = runSimpleUniqueMonad freshLabel
