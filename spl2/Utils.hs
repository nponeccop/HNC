
module Utils where

import qualified Data.ByteString
import Data.Char
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

ll l = [last l]
ff (l :_) = [l]

trace2 x = trace (show x) x

fromRight v = case v of
	Right r -> r
	Left l -> error $ show l

constructJust cond val = if cond then Nothing else Just val

deconstructJust (Just j) _ = j
deconstructJust Nothing val = val

packL = Data.ByteString.pack . map (fromIntegral . ord)

subtractSet x y = S.difference x (S.intersection x y)

subtractMap x y = M.difference x (M.intersection x y)
subtractKeysFromMap m keyList = foldl (flip M.delete) m keyList

joinStr _ [] = ""
joinStr sep l = foldl1 (\x y -> x ++ sep ++ y) l

mapAndJoinStr _ [] = []
mapAndJoinStr x l = foldl1 (++) $ map x l

inStrings l r x = l ++ x ++ r

-- TODO refactor Utils.showJoinedList and Utils.showJoinedList2 to extract
showJoinedList separator = joinStr separator . map show

showJoinedList2 separator = mapAndJoinStr (\x -> show x ++ separator)

uncondLookup k m = maybe (error $ "Utils.uncondLookup: cannot find " ++ show k ++ " in " ++ show m) id $ M.lookup k m
