{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils where

import qualified Data.ByteString
import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Debug.Trace
import Test.HUnit


liftM3M f a b c = join $ liftM3 f a b c

st testName expected actual = TestLabel testName $ TestCase $ assertEqual "" expected actual
simpleTests x = runTestTT $ TestList x

xtrace _ b = b
ztrace m t =  trace (m ++ " = " ++ show t) t
xeqTrace _ _ y = y
zeqTrace m x y = if x == y then y else trace (m ++ ": " ++ show x ++ " /= " ++ show y) y


consMaybe Nothing value = value
consMaybe (Just x) value = x : value

fromRight v = case v of
	Right r -> r
	Left l -> error $ show l

constructJust cond val = if cond then Nothing else Just val

deconstructJust (Just j) _ = j
deconstructJust Nothing val = val

packL = Data.ByteString.pack . map (fromIntegral . ord)

joinStr _ [] = ""
joinStr sep l = foldl1 (\x y -> x ++ sep ++ y) l

inStrings l r x = l ++ x ++ r

-- TODO refactor Utils.showJoinedList and Utils.showJoinedList2 to extract
showJoinedList separator = joinStr separator . map show

showJoinedList2 separator = concatMap (\x -> show x ++ separator)
inParens = inStrings "(" ")"
inAngular = inStrings "<" ">"

uncondLookup = tracedUncondLookup "No trace"

tracedUncondLookup msg k m = fromMaybe (error $ msg ++ ": uncondLookup cannot find " ++ show k ++ " in " ++ show m) $ M.lookup k m