module Graph.Hcub (
  hcub,
  hcubConfig,
  hcubShallowConfig
) where

import Prelude

import Graph.Aop
import Graph.SingleConstantMult
import Graph.Util
import Graph.CSD

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List (sortBy, sort, minimumBy)
import Data.Maybe (listToMaybe, fromMaybe, fromJust, isJust)
import Data.Ratio
import Control.Monad.Reader


hcubConfig :: [Fundamental] -> IO (McmConfig AOdd)
hcubConfig ws =
  let bits = ceiling $ logBase 2 (fromIntegral $ maximum ws)
      constr = AOdd $ bits + 1
      lut  = lowCostGraphs constr [const 0]
      depth = Nothing
  in pure $ McmConfig constr depth lut ws

hcubShallowConfig :: [Fundamental] -> IO (McmConfig AOdd)
hcubShallowConfig ws = do
  depthLut <- loadLUT "./LutMinDepth.bin"
  (McmConfig constr depth lut ws) <- hcubConfig ws
  let depth' = maximum . map estMinDepth $ divEvens ws
      estMinDepth w = minimum $ csdDepth w : map (depthOfFundamental w) (allGraphsForCoef depthLut w)
  return $ McmConfig constr (Just depth') lut ws

hcub :: Reader (McmConfig AOdd) Graph
hcub = do
  cfg <- ask
  let t = Graph . nothingMap . filter (>1) . divEvens $ ws cfg
      r = Graph $ Map.singleton 1 (Node Nothing 0)
      w = Graph $ Map.singleton 1 (Node Nothing 0)
      s = Graph $ Map.singleton 1 (Node Nothing 0)
      dists  = Map.map (\_->(maxBound, Map.empty)) $ unGraph t
  return $ loopHcub cfg (dists, (r, w, s, t))

nothingMap = Map.fromList . map (\t->(t,Node Nothing 0))

loopHcub :: McmConfig AOdd
         -> ( DistCache
            , (Graph, Graph, Graph, Graph) )
         -> Graph
loopHcub cfg (dists, gs@(r, w, s, t))
  -- If T is empty, return R
  | Map.null (unGraph t) = r
  -- Else, chain optimalSteps and a heuristicStep then recurse
  | otherwise  = loopHcub cfg . heuristicStep cfg dists $ optimalStep cfg gs

optimalStep ::  McmConfig AOdd
            -> (Graph, Graph, Graph, Graph)
            -> (Graph, Graph, Graph, Graph)
optimalStep cfg (Graph r, Graph w, Graph s, Graph t)
  | Map.null w = (Graph r, Graph w, Graph s, Graph t)
  | otherwise  = let r' = Map.union r w
                     s' = unGraph
                         -- . constrainDepth maxDepth (Graph r)
                          $ incrementalSuccSet cfg (Graph r') (Graph w) (Graph s)
                     w' = s' `Map.intersection` t
                     t' = t `Map.difference` s'
                 in optimalStep cfg (Graph r', Graph w', Graph s', Graph t')

heuristicStep :: McmConfig AOdd
              -> DistCache
              -> (Graph, Graph, Graph, Graph)
              -> ( DistCache
                 , (Graph, Graph, Graph, Graph) )
heuristicStep cfg dists (Graph r, Graph w, Graph s, Graph t)
  | Map.null t = (dists, (Graph r, Graph w, Graph s, Graph t))
  | otherwise  = let (dists', (newKey, newVal)) = hcubMetric cfg dists (Graph r, Graph s, Graph t)
                     t' = Map.delete newKey t
                     w' = Map.insert newKey newVal w
                     dists'' = updateEstimatedDistRt newKey (Graph t) dists'
                 in (dists'', (Graph r, Graph w', Graph s, Graph t'))

hcubMetric :: McmConfig AOdd -> DistCache -> (Graph, Graph, Graph) -> ( DistCache, (Fundamental, Node))
hcubMetric cfg dists (Graph r, Graph s, Graph t) =
  let cub (s,_) = sum $ map (bBar s) (Map.keys t)
      bBar    s  t = 10^^(negate $ (snd $ dists' Map.! t) Map.! s) * fromIntegral (max 0 $ (fst $ dists' Map.! t) - ((snd $ dists' Map.! t) Map.! s))
      dists' = estimateDist cfg (Graph r) (Graph s) (Graph t) dists
      --cubChoices = sortBy (\(_,a) (_,b) -> compare a b) . sortBy (\((a,_),_) ((b,_),_)-> compare a b) . map (\a->(a,cub a)) $ Map.toList s
      choices = pickWithMaxMetric cub $ Map.toList s
     -- Secondary metric here is NOFS (non-output fundamental sum, i.e.
     -- verticeSum) Easier to compare results with this, plus integrating random
     -- selection is a bit of a ball breaker
  in (dists', minimumBy (\a b -> compare (fst a) (fst b)) $ choices)
  --in (dists', last $ map fst cubChoices)
