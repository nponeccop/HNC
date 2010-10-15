module HN.Optimizer.Utils where
import Test.HUnit
import HN.Optimizer.Intermediate
import Debug.Trace

xtrace a b = trace (a ++ " = " ++ show b) b

ci int = Constant $ ConstInt int
sv name int = Definition name [] $ In $ ci int


st testName expected actual = TestLabel testName $ TestCase $ assertEqual "" expected actual
simpleTests x = runTestTT $ TestList x

data T n e def = T n e def

stt t2f = map $ \(T n e def) -> st n e $ t2f def
