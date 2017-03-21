module Test.Utils where
import Test.HUnit
import HN.Intermediate
import Debug.Trace

xtrace a b = trace (a ++ " = " ++ show b) b

ci int = Constant $ ConstInt int
sv name int = Definition name [] $ In $ ci int

st testName expected actual = testName ~: expected ~=? actual

stt t2f = map (\(n, e, def) -> n ~: e ~=? t2f def)
