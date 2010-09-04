module Utils where

import qualified Data.ByteString
import Data.Char
import qualified Data.Map as M
import Debug.Trace

xtrace a b = b
ztrace m t =  trace (m ++ " = " ++ show t) t

consMaybe Nothing value = value
consMaybe (Just x) value = x : value

fromRight v = case v of
	Right r -> r
	Left l -> error $ show l

constructJust cond val = if cond then Nothing else Just val

deconstructJust (Just j) _ = j
deconstructJust Nothing val = val

packL = Data.ByteString.pack . map (fromIntegral . ord)

subtractKeysFromMap m keyList = foldl (flip M.delete) m keyList

joinStr _ [] = ""
joinStr sep l = foldl1 (\x y -> x ++ sep ++ y) l

inStrings l r x = l ++ x ++ r

-- TODO refactor Utils.showJoinedList and Utils.showJoinedList2 to extract
showJoinedList separator = joinStr separator . map show

showJoinedList2 separator = concatMap (\x -> show x ++ separator)
inParens x = inStrings "(" ")" x
inAngular x = inStrings "<" ">" x

uncondLookup k m = tracedUncondLookup "No trace" k m

tracedUncondLookup msg k m = maybe (error $ msg ++ ": uncondLookup cannot find " ++ show k ++ " in " ++ show m) id $ M.lookup k m