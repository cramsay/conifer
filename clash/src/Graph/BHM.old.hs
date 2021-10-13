module Graph.BHM (
  pickBHM
) where

import Prelude

import Graph.AdderGraphs
import Graph.Util

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bits
import Data.List (sort, sortBy, minimumBy)
import Data.Maybe

pickBHM :: LookupGraphs -> [Coef] -> Graph
pickBHM lut ws = foldl step (Graph $ Map.singleton 1 Nothing) ts_dropping
  where
    ts = sortBy (\a b-> compare (costOf lut a) (costOf lut b)) $ sort (divEvens ws)
    ts_dropping = take (length ts) $ iterate tail ts
    step acc elem = buildTarget elem acc (head elem)

--def bhm_rec_step(T,R,mincT,error):
buildTarget :: [Coef] -> Graph -> Int -> Graph
buildTarget ts (Graph rs) error
--    # Base case on R containts mincT
  | Map.member mincT rs = Graph rs

--    # Case for fundamental of `error` exists in R 
--    # Only synthesize rc + error
  | Map.member (divEven $ abs error) rs = let edges = orderEdges (decomposeToEdge error)
                                                                 (decomposeToEdge rc   )
                                          in Graph $ Map.insert mincT (Just edges) rs
--    # Recusive case ---
--    # Try to generate the best approximation of error and rc+best
  | otherwise = let s1 = head . pickWithMetric' (minError error) $ Map.toList ss
                    rsWithS1 = Map.insert (fst s1) (snd s1) rs
                    error' = if error > 0 then error - (fst s1) else error + (fst s1)
                    rs' = if rc == 0 then rsWithS1 else
                          let sgn = if error > 0 then POS else NEG
                              e1  = Edge (fst s1) sgn 0
                              e2  = decomposeToEdge rc
                          in Map.insert (calcCoef e1 e2) (Just (e1,e2)) rsWithS1
                in buildTarget ts (Graph rs') error'
-- Some common definitions
  where
    mincT = head ts
    rc = mincT - error
    (Graph ss) = succSet (maximum ts) (Graph rs)

-- Generate a graph containing A∗(R, R) (all fundamentals with an A-distance of 1 from R)
succSet :: Coef -> Graph -> Graph
-- TODO I'm returning succset U R, because the example '7' '27' '39' '45' seems to need it.
-- Is this sane? or does the example with SPIRAL break BHM?
succSet maxT (Graph g) = Graph . Map.union g $ foldr addFund Map.empty
                         [ (coef, Just edges)
                         | f1 <- Map.keys g
                         , f2 <- Map.keys g
                         , (sg1, sg2) <- [(POS,POS), (POS,NEG)]
                         , let maxShift = ceiling $ logBase 2 (fromIntegral $ 2*maxT)
                         , (sh1, sh2) <- zip [0..maxShift] (repeat 0) ++
                                         zip (repeat 0   ) [0..maxShift]
                         , let e1 = Edge f1 sg1 sh1
                         , let e2 = Edge f2 sg2 sh2
                         -- Calculate coef
                         , let coef = calcCoef e1 e2
                         , let edges = orderEdges e1 e2
                         , coef > 0
                         , coef < 2*maxT
                         , Map.notMember coef g -- Ensure the new coef doesn't exist in R
                         ]
  -- Collect up all results of adding a new A-op
  -- Just reuse Graph here to get uniqueness for free (it's a map of coefs!)
  -- Remove anything that existed in g, then return
  where
  addFund (coef, es) g = Map.insert coef es g

minError :: Coef -> (Coef, a) -> Int
minError e (c, _) = abs $ e - c

pickWithMetric' :: ((Coef,a) -> Int) -> [(Coef,a)] -> [(Coef,a)]
pickWithMetric' metric  gs =
  let bestMetric = minimum $ map metric gs
  in filter ((bestMetric ==) . metric) gs
