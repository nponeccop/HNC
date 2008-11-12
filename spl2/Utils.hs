
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