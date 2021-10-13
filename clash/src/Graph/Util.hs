module Graph.Util where

import Prelude

import Graph.Aop
import Graph.SingleConstantMult
import Graph.CSD

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List (sortBy, nub)
import Data.Maybe (fromMaybe, catMaybes, isJust, fromJust)
import Data.Bits

data McmConfig constr = McmConfig { aopConstr :: constr
                                  , maxDepth  :: Maybe Int
                                  , lut       :: SCMLookup
                                  , ws        :: [Fundamental]
                                  }

-- | Adder cost metric --- the number of A-ops in the graph
adderCostMetric :: Metric
adderCostMetric =  (\sz->sz-1) . Map.size . unGraph

-- | Vertice sum metric --- the arithmetic sum of all fundamentals in the graph
verticeSumMetric :: Metric
verticeSumMetric = sum . Map.keys . unGraph

-- | Depth metric --- the total depth of the graph
depthMetric :: Metric
depthMetric (Graph g) = (depthFrom 1) - 1
  where depthFrom c = let children = Map.keys $ Map.filter (isChildOf c) g
                      in  1 + maximum (0 : map depthFrom children)
        isChildOf c = fromMaybe False . fmap (\(AOp _ a b)->a==c || b ==c) . op

-- | Helper function to filter a set of graphs by a given metric (lower is
-- better), keeping only those with the best score.
pickWithMetric :: (Ord a, Eq a)
               => (b->a) -> [b] -> [b]
pickWithMetric metric  gs =
  let withMetrics = map (\g->(g,metric g)) gs
      bestMetric = minimum $ map snd withMetrics
  in map fst $ filter ( (bestMetric == ) . snd ) withMetrics

-- | Helper function to filter a set of graphs by a given metric (higher is
-- better), keeping only those with the best score.
pickWithMaxMetric :: (Ord a, Eq a)
               => (b->a) -> [b] -> [b]
pickWithMaxMetric metric  gs =
  let withMetrics = map (\g->(g,metric g)) gs
      bestMetric = maximum $ map snd withMetrics
  in map fst $ filter ( (bestMetric == ) . snd ) withMetrics


-- | Get the depth of a given fundamental
depthOfFundamental :: Fundamental -> Graph -> Int
depthOfFundamental c (Graph g) = depth $ (g Map.! c)

-- | Return all stored SCM graphs for a given fundamental. These graphs will
-- include one 'best' graph for each possible cost, as selected by the metrics
-- supplied when generating the lookup
allGraphsForCoef :: SCMLookup -> Fundamental -> [Graph]
allGraphsForCoef lut c = catMaybes . Map.elems $ Map.map (\cg -> cg Map.!? c) lut


-- | Change the order of (u,v) for all nodes to ensure that the abs in the
-- A-operation is unneccesary. This means that we don't have to generate
-- hardware for the abs.
avoidAbs :: Graph -> Graph
avoidAbs (Graph g) = Graph $ Map.map reorder g
  where
    -- No swap for input node
    reorder n@(Node Nothing d) = n
    -- No swap for additions
    reorder n@(Node (Just (AOp (AConf _ _ _ POS) _ _)) d) = n
    -- Swap for subtractions if u << l1 < v << l2
    reorder n@(Node (Just (AOp (AConf l1 l2 r NEG) u v)) d) = case shiftL u l1 > shiftL v l2 of
      True -> n
      False -> Node (Just (AOp (AConf l2 l1 r NEG) v u)) d

type Depth = Int

isValidDepth :: Maybe Depth -> Depth -> Bool
isValidDepth maxD d = fromMaybe True $ fmap (\max -> d <= max) maxD

betterDepth :: Node -> Node -> Node
betterDepth n1 n2
  | depth n2 < depth n1 = n2
  | otherwise           = n1


-- | Successor set of a set of fundamentals and a single fundamental
succSetSingle :: AOpConstr constr => McmConfig constr -> Graph -> (Fundamental, Depth) -> Graph
succSetSingle cfg (Graph g) (f1,d1) =
  let newFunds = [ (f,Node (Just aop) d)
                 | f2     <- Map.keys g
                 , (f,aop) <- aStarSet (aopConstr cfg) f1 f2
                 --, Map.notMember f g && f /= f1
                 -- TODO Removed this line to make dist 3 case 5 test work... should I do this more elegantly?
                 , let d = 1 + max d1 (depthOfFundamental f2 $ Graph g)
                 , isValidDepth (maxDepth cfg) d
                 ]
  in Graph $ foldr (uncurry (Map.insertWith betterDepth)) Map.empty newFunds

-- | Successor set of two sets of fundamentals
succSet :: AOpConstr constr => McmConfig constr -> Graph -> Graph -> Graph
succSet cfg (Graph g1) (Graph g2) =
  let newFunds = [ (f,Node (Just aop) d)
                 | f1     <- Map.keys g1
                 , f2     <- Map.keys g2
                 , (f,aop) <- aStarSet (aopConstr cfg) f1 f2
                 , Map.notMember f g1 && Map.notMember f g2
                 -- TODO Removed this line to make dist 3 case 5 test work... should I do this more elegantly?
                 , let d = 1 + max (depthOfFundamental f1 $ Graph g1)
                                   (depthOfFundamental f2 $ Graph g2)
                 , isValidDepth (maxDepth cfg) d
                 ]
  in Graph $ foldr (uncurry (Map.insertWith betterDepth)) Map.empty newFunds

-- | Incrementally build a successor set from ready, working, and old successor sets
-- | See HCUB paper Eqn. 14
incrementalSuccSet :: AOpConstr constr => McmConfig constr -> Graph -> Graph -> Graph -> Graph
incrementalSuccSet cfg (Graph r) (Graph w) (Graph s) = Graph $
  (Map.unionWith (\a b -> if depth b < depth a then b else a)
   s
   (unGraph $ succSet cfg (Graph $ Map.union r w) (Graph w))
  ) `Map.difference` w

-- | Helper to generate all cost 1 fundamentals
c1s :: SCMLookup -> [Fundamental]
c1s lut = Map.keys (lut Map.! 1)

-- | Helper to generate all cost 1 fundamentals
c2s :: SCMLookup -> [Fundamental]
c2s lut = Map.keys (lut Map.! 2)

-- | Helper for finding integer divisions of a fundamental set
divBy :: Fundamental -> [Fundamental] -> [Fundamental]
divBy u vs = [ divEven $ u `div` v
             | v <- vs
             , 0 == u `mod` v
             ]

toMaybeGraph :: Map Fundamental Node -> Maybe Graph
toMaybeGraph g
  | Map.null g = Nothing
  | otherwise = Just (Graph g)

toNothingGraph :: [Fundamental] -> Graph
toNothingGraph ks = Graph . Map.fromList $ zip ks (repeat $ Node Nothing 0)

constrainDepth :: Maybe Depth -> Map Fundamental Node -> Map Fundamental Node
constrainDepth maxD = Map.filter (\(Node _ d) -> fromMaybe True (fmap (d<=) maxD))


-- TODO These distance tests work only for AOdd, so it doesn't make sense to have them for generic AOpConstrs
--      Should we even have a AOpConstr type class?

-- Distance tests
----------------------------------------
-- See Hcub paper Table III for summary of the cases for each distance

isDistance1 :: AOpConstr constr => McmConfig constr -> Graph -> Graph -> Fundamental -> Maybe Graph
isDistance1 _ (Graph r) (Graph s) t = Graph . Map.singleton t <$> Map.lookup t s
  -- We're assuming that successor set is already depth constrained

isDistance2 :: AOpConstr constr => McmConfig constr -> Graph -> Graph -> Fundamental -> Maybe Graph
isDistance2 cfg (Graph r) (Graph s) t =
  let
      r'    = constrainDepth (fmap (\a->a-1) $ maxDepth cfg) r
      aStarRt = unGraph $ succSetSingle cfg (Graph r') (t,0)
      s'    = constrainDepth (fmap (\a->a-1) $ maxDepth cfg) s
      case1 = Map.intersection s' aStarRt
      tByC1 = unGraph $ toNothingGraph (divBy t . c1s $ lut cfg)
      case2 = Map.intersection s' tByC1
  in  toMaybeGraph $ case1 `Map.union` case2

isDistance3 :: AOpConstr constr => McmConfig constr -> Graph -> Graph -> Fundamental -> Maybe Graph
isDistance3 cfg (Graph r) (Graph s) t =
  let tByC1 = unGraph $ toNothingGraph (divBy t . c1s $ lut cfg)
      tByC2 = unGraph $ toNothingGraph (divBy t . c2s $ lut cfg)
      r'    = constrainDepth (fmap (\a->a-1) $ maxDepth cfg) r
      r''   = constrainDepth (fmap (\a->a-2) $ maxDepth cfg) r'
      aStarRt = unGraph $ succSetSingle cfg (Graph r') (t,0)
      aStarS't = unGraph $ succSetSingle cfg (Graph s') (t,0)
      deeperCfg = cfg {maxDepth = (+1) <$> maxDepth cfg }
      justMaxDepth = fromMaybe maxBound (maxDepth cfg)
      sMaxes = Graph $ Map.filter (\n->
                 depth n == justMaxDepth && (
                 fromMaybe False ( (\x -> depth (r Map.! u x) <= justMaxDepth - 2) <$> op n) ||
                 fromMaybe False ( (\x -> depth (r Map.! v x) <= justMaxDepth - 2) <$> op n) )
               ) s
      aStarSMaxest  = unGraph $ succSetSingle deeperCfg sMaxes (t,0)
      s'      = constrainDepth (fmap (\a->a-1) $ maxDepth cfg) s
      s''     = constrainDepth (fmap (\a->a-2) $ maxDepth cfg) s'
      case1_2 = Map.intersection s'' tByC2
      case3   = Map.intersection s'' (unGraph $ succSet cfg (Graph tByC1) (Graph r''))
      case4   = Map.intersection s'' (Map.fromList [
                                       (v,Node Nothing 0)
                                       | u <- Map.keys aStarRt
                                       , v <- divBy u (c1s $ lut cfg)
                                     ])
      case5   = Map.intersection s' aStarS't
      case5iso   = Map.intersection s'' aStarSMaxest
  in  toMaybeGraph $ Map.unions [case1_2
                                ,case3
                                ,case4
                                ,case5
                                ,case5iso]

type Distance = Int

distEst :: AOpConstr constr => McmConfig constr -> Distance -> (Fundamental,Distance) -> Fundamental -> Distance
distEst cfg distRt (s, sDepth) t =
  let tByC1 = toNothingGraph (divBy t . c1s $ lut cfg)
      csdCostWithDepth n z = if isValidDepth (fmap (\a->a-n) $ maxDepth cfg) (csdDepth z - 1) then csdCost z else maxBound
      saturateAdd small big = if small <= maxBound - big then big + small else maxBound
      -- Be very wary of arithmetic on distance estimates that might be maxBound! maxBound + 1 is a huge negative number
      e1 = minimum $ maxBound : map (saturateAdd 1) [ csdCostWithDepth 1 z | (z,_) <- aStarSet (aopConstr cfg) t s]
      e2 = minimum $ maxBound : map (saturateAdd 2) [ csdCostWithDepth 2 z |  z    <- Map.keys . unGraph $ succSetSingle cfg tByC1 (s,0)]
      e3 = minimum $ maxBound : map (saturateAdd 2) [ csdCostWithDepth 1 z |  c1   <- c1s $ lut cfg
                                                     ,  maxF (aopConstr cfg) >= c1*s
                                                     ,  (z,_) <- aStarSet (aopConstr cfg) (c1*s) t]
      e1' = if isValidDepth ((\a->a-1) <$> maxDepth cfg) sDepth then e1 else maxBound
      e2' = if isValidDepth ((\a->a-2) <$> maxDepth cfg) sDepth then e2 else maxBound
      e3' = if isValidDepth ((\a->a-2) <$> maxDepth cfg) sDepth then e3 else maxBound
      -- Leave case 4 out because it is in paper but not in Hcub source code
  in minimum [distRt, e1', e2', e3']
  -- TODO is there the potential for cached distances to stick around even after we know the solution would be too deep?
  --    ^ This isn't the case for the first iteration though, so keep looking

type DistT     = (Distance, Map Fundamental Distance)
type DistCache = Map Fundamental DistT

-- Build distance estimates for between R and t, and all R + s and t

estimateDistT :: AOpConstr constr
             => McmConfig constr -> Graph -> Graph -> Fundamental -> DistT -> DistT
estimateDistT cfg r s t (distT, cache)
  | isJust d1s = createCache 1 d1s
  | isJust d2s = createCache 2 d2s
  | isJust d3s = createCache 3 d3s
  | otherwise  = (distT,
                  Map.unionWith min cache $
                  Map.fromList [ (suc,dist)
                               | suc <- Map.keys $ Map.difference (unGraph s) cache
                               , let sucDepth = depth ((unGraph s) Map.! suc)
                               , let dist = distEst cfg
                                             (fromMaybe maxBound $ Map.lookup suc cache)
                                             (suc, sucDepth) t
                  ])
  where d1s = (isDistance1 cfg r s t)
        d2s = (isDistance2 cfg r s t)
        d3s = (isDistance3 cfg r s t)
        createCache dist set = let closers = Map.map (const (dist-1)) .
                                             unGraph $ fromJust set
                                   others  = Map.map (const  dist)    .
                                             Map.difference (unGraph s) .
                                             unGraph $ fromJust set
                               in (dist, Map.unionWith min (closers `Map.union` others) cache)

estimateDist :: AOpConstr constr
             => McmConfig constr -> Graph -> Graph -> Graph -> DistCache -> DistCache
estimateDist cfg r s (Graph t) ests = Map.foldrWithKey (\k _ cin -> Map.adjust (estimateDistT cfg r s k) k cin)
                                             ests
                                             t

updateEstimatedDistRt :: Fundamental -> Graph -> DistCache -> DistCache
updateEstimatedDistRt s (Graph t) cache =
  -- For every t in t, find the correspending s distance and set tdist to this if min
  Map.foldrWithKey (\t' _ dcache -> Map.adjust (\(tdist, sdists)->(min tdist (sdists Map.! s), sdists)) t' dcache) cache t

-- TODO Try switching to IntMap for faster performance
