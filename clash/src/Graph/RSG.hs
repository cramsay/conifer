module Graph.RSG (
  rsg,
  rsgConfig
) where

import Prelude

import Graph.Aop
import Graph.SingleConstantMult
import Graph.Util

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List (sortBy)
import Control.Monad.Reader

pickFundamentalGraphRSG :: SCMLookup -> Fundamental -> Graph
pickFundamentalGraphRSG lut c =
  let cost = minCostOf lut c
      gs = allGraphsForCoef lut c
  in head . pickWithMetric verticeSumMetric
          $ pickWithMetric depthMetric      gs

rsg :: Reader (McmConfig AOdd) Graph
rsg = do
  cfgLut <- asks lut
  cfgWs  <- asks ws
  let targets = reverse $ sortBy (\a b-> compare (minCostOf cfgLut a) (minCostOf cfgLut b)) (divEvens cfgWs)
      takeShallowest a b = if depth b < depth a then b else a
  return . Graph . foldr (Map.unionWith takeShallowest) Map.empty
                 $ map (unGraph . pickFundamentalGraphRSG cfgLut) targets

rsgConfig :: [Fundamental] -> IO (McmConfig AOdd)
rsgConfig ws = do
  let bits = ceiling $ logBase 2 (fromIntegral $ maximum ws)
      constr = AOdd $ bits + 1
      depth = Nothing
  lut <- loadLUT "./LutMinDepth.bin"
  return $ McmConfig constr depth lut ws

-- 1) They use the graph extensions to compute up to 19 bits
-- 2) They store all possible graphs (not just optimal costs) up to cost 4.
--    -> How does this play with then picking the shallowest one?!
-- 3) When picking a MAG they filter by best depth, then adder vertice
