module HN.MilnerTools (instantiatedType, freshAtoms) where
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import HN.TypeTools
import SPL.Types

-- freshAtoms используется всего в одном месте - при
-- вычислении атрибута Definition.loc.argAtoms
freshAtoms :: [String] -> Int -> (Int, [(String, T)])
freshAtoms [] counter = (counter, [])
freshAtoms a counter = (counter + length a, zipWith (\a i -> (a, tv i)) a [counter..])

instantiatedType counter (tu, t) = (counter + S.size tu, mapTypeTV (\a -> fromMaybe (TV a) (M.lookup a substitutions)) t) where
	substitutions = M.fromDistinctAscList $ zipWith (\a b -> (a, tv b)) (S.toAscList tu) [counter..]
