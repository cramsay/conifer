module Graph.RAGn (
  ragn,
  ragnConfig
) where

import Prelude

import Graph.Aop
import Graph.SingleConstantMult
import Graph.Util

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List (sortBy, sort)
import Data.Maybe (listToMaybe, fromMaybe, fromJust, isJust)
import Control.Monad.Reader

ragnConfig :: [Fundamental] -> IO (McmConfig AOdd)
ragnConfig ws = do
  let bits = ceiling $ logBase 2 (fromIntegral $ maximum ws)
      constr = AOdd $ bits + 1
      depth = Nothing
  lut <- loadLUT "./LutMinCost.bin"
  return $ McmConfig constr depth lut ws

nothingMap = Map.fromList . map (\t->(t,Node Nothing 0))

-- TODO We could improve performance by using incrementalSucSet, as used in Hcub

-- | Add all targets that are in the successor set
addAllInS :: McmConfig AOdd -> Graph -> [Fundamental] -> (Graph, [Fundamental])
addAllInS cfg g ts = let (Graph ss) = succSet cfg g g
                         tSet       = nothingMap ts
                         ts'        = Map.keys $ Map.difference tSet ss
                         g'         = Map.union (unGraph g) $ Map.intersection ss tSet
                     in (Graph g', ts')

-- | Try to select a target of distance 2
-- | We can assume there are no targets of distance 1 present
selectFromS2 :: McmConfig AOdd -> Graph -> [Fundamental] -> Maybe (Graph, [Fundamental])
selectFromS2 cfg g ts = let (Graph ss)  = succSet cfg g g
                            tSet        = nothingMap ts
                            pickFromS  (Graph s) = (Graph $ uncurry (Map.insert) (head $ Map.toList s) (unGraph g), ts)
                        in listToMaybe [ pickFromS $ fromJust mOptions
                                       | t<-ts
                                       , let mOptions = isDistance2 cfg g (Graph ss) t
                                       , isJust mOptions
                                       ]

-- | Just add the best SCM graph for the cheapest target, and 
addBestMAG :: McmConfig AOdd -> Graph -> [Fundamental] -> (Graph, [Fundamental])
addBestMAG cfg g cs = let c = head . sortBy cmpCost $ sort cs
                          cs' = filter (/=c) cs
                      in ( Graph . Map.union (unGraph g) . unGraph $ (lut cfg) Map.! minCostOf (lut cfg) c Map.! c -- We take graph of minimum cost... for RAGn results the lut must be generated with the [verticeSumMertic] metric
                         , cs'
                         )
  where cmpCost a b = compare (minCostOf (lut cfg) a) (minCostOf (lut cfg) b)


loopRAGn :: McmConfig AOdd -> Graph -> [Fundamental] -> Graph
loopRAGn _ g [] = g -- Terminate on empty coef list
loopRAGn cfg g cs
  | snd with1s /= cs = uncurry (loopRAGn cfg) with1s -- Add any targets from the successor
  | isJust withA2    = uncurry (loopRAGn cfg) $ fromJust withA2 -- Found a distance 2, so let's start over
  | otherwise        = uncurry (loopRAGn cfg) withAMAG -- Use a cost >2 and start over
  where with1s = addAllInS cfg g cs -- Add all dist 1s
        withA2 = selectFromS2 cfg g cs --uncurry (addD2 lut) with1s -- Lazily add a dist 2 -- TODO add distance 2s
        withAMAG = uncurry (addBestMAG cfg) with1s -- Lazily add a best costed other

ragn :: Reader (McmConfig AOdd) Graph
ragn = do
  cfg    <- ask
  let targets = filter (>1) . divEvens $ ws cfg
      g       = Graph $ Map.singleton 1 (Node Nothing 0)
  return $ loopRAGn cfg g targets
