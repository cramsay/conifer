{-# LANGUAGE DeriveGeneric #-}

module Graph.SingleConstantMult where

import Prelude

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bits
import Data.List
import Data.Maybe
import GHC.Generics (Generic)
import Control.Applicative (liftA2)
import Data.Binary -- (encodeFile, decodeFile, Binary)
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as B

import Graph.Aop

type Cost = Int

data Node = Node {op :: Maybe AOp, depth :: Int}
  deriving (Eq, Show, Generic)

newtype Graph = Graph {unGraph :: Map Fundamental Node}
  deriving (Eq, Show, Generic)

instance Binary Node
instance Binary Graph
instance Binary Sign
instance Binary AConf
instance Binary AOp

type SCMGraphs = Map Fundamental Graph
type SCMLookup = Map Cost SCMGraphs

type Metric = Graph -> Int
-- Hide GraphSelector
type GraphSelector = Graph -> Graph -> Graph

-- TODO Don't constrain by cost, constraint by depth?

-- | Helper function to select a `best` graph given an ordered list of metrics
selectByMetrics :: [Metric] -> Graph -> Graph -> Graph
selectByMetrics [] g1 g2 = g1
selectByMetrics (m:ms) g1 g2 = case compare (m g1) (m g2) of
  EQ -> selectByMetrics ms g1 g2
  LT -> g1
  GT -> g2

-- | Get the minimum SCM cost for a given fundamental
minCostOf :: SCMLookup -> Fundamental -> Cost
minCostOf lut f = fromMaybe maxBound $
  Map.foldrWithKey
  (\k costKs acc -> if Map.member f costKs then Just k else acc)
  Nothing lut

-- | Get the minimum SCM cost for a given fundamental
minCostDepthOf :: SCMLookup -> Fundamental -> Int
minCostDepthOf lut f = let cost = minCostOf lut f
                       in if cost < maxBound then
                             depth $ (unGraph $ lut Map.! minCostOf lut f Map.! f) Map.! f
                          else maxBound

labelDepths :: Graph -> Graph
labelDepths (Graph g) = Graph $ Map.mapWithKey (\k a -> Node (op a) (calcDepth (Graph g) k)) g

calcDepth (Graph g) k
  | Map.notMember k g = error $ "key not in graph: " ++ show k -- maxBound
  | otherwise         = let ps = op $ g Map.! k
                        in  1 + max (parentDepth $ u <$> ps)
                                    (parentDepth $ v <$> ps)
  where parentDepth p = case p of
          Nothing -> -1
          Just p  -> calcDepth (Graph g) p

-- | Insert all new coefs representing the addition of two existing coefs.
-- | This is just the union of the two parent graphs plus permutations of a new
-- | vertex
insertAdds
  :: AOpConstr constr
  => constr
  -> GraphSelector   -- ^ Select to retain a single "best" graph
  -> SCMLookup  -- ^ Input lookup
  -> (Cost, Fundamental)  -- ^ 1st parent coef
  -> (Cost, Fundamental)  -- ^ 2nd parent coef
  -> Cost          -- ^ Resulting cost
  -> SCMLookup  -- ^ Output lookup
insertAdds constr select lut p1 p2 cost =
  let  baseG = Map.union (unGraph $ lut Map.! (fst p1) Map.! (snd p1))
                         (unGraph $ lut Map.! (fst p2) Map.! (snd p2))
       depth = 1+ max (minCostDepthOf lut $ snd p1)
                      (minCostDepthOf lut $ snd p2)
       newGraphs = map (\(f, node) -> (f, labelDepths . Graph $ Map.insert f node baseG)) $
                   filterMembers $
                   [ (f, Node (Just op) maxBound)
                   | (f,op) <- aStarSet constr (snd p1) (snd p2)
                   -- , minCostOf lut f >= cost || minCostDepthOf lut f >= depth
                   , minCostDepthOf lut f >= depth
                   ]
       addBetterGraph (coef, g) = Map.insertWith select coef g
       addGraphs scmGs = foldr addBetterGraph scmGs newGraphs
       filterMembers = filter (flip Map.notMember baseG . fst ) -- Need to rule
                                                                -- out existing
                                                                -- coefficients
                                                                -- as new
                                                                -- additions to
                                                                -- keep the
                                                                -- graph acyclic
  in Map.adjust addGraphs cost lut

---- | Insert a new coef representing the multiplication of two existing coefs.
---- | This is the cascade of two parent graphs
insertMults
  :: AOpConstr constr
  => constr
  -> GraphSelector   -- ^ Select to retain a single "best" graph
  -> SCMLookup  -- ^ Input lookup
  -> (Cost, Fundamental)  -- ^ 1st parent coef
  -> (Cost, Fundamental)  -- ^ 2nd parent coef
  -> Cost          -- ^ Resulting cost
  -> SCMLookup  -- ^ Output lookup
insertMults constr select lut p1 p2 cost =
  let  (Graph g1) = lut Map.! (fst p1) Map.! (snd p1)
       (Graph g2) = lut Map.! (fst p2) Map.! (snd p2)
       depth = (minCostDepthOf lut $ snd p1) + (minCostDepthOf lut $ snd p2)
       coef       = snd p1 * snd p2
       g' = Map.union g1 $
            Map.map (\(Node aops d)->Node (fmap (multNode (snd p1)) aops) d) $
            Map.mapKeys ((snd p1)*) g2
       multNode c (AOp cfg f1 f2) = AOp cfg (c*f1) (c*f2)
       addBetterGraph = Map.insertWith select coef . labelDepths $ Graph g'
  in if coef < maxF constr
        -- && (minCostOf lut coef >= cost || minCostDepthOf lut coef >= depth)
        && (minCostDepthOf lut coef >= depth)
     then Map.adjust addBetterGraph cost lut
     else lut

insertLeapfrog2s
  :: AOpConstr constr
  => constr
  -> GraphSelector   -- ^ Select to retain a single "best" graph
  -> SCMLookup  -- ^ Input lookup
  -> (Cost, Fundamental)  -- ^ 1st parent coef
  -> (Cost, Fundamental)  -- ^ 2nd parent coef
  -> Cost          -- ^ Resulting cost
  -> SCMLookup  -- ^ Output lookup
insertLeapfrog2s constr select lut p1 p2 cost =
  let newGraphs = [ (l2, labelDepths $ Graph g')
                  -- For all pairs with p1 and 1
                  | (l1, l1Op) <- aStarSet constr (snd p1) 1
                  -- prod = l1 * p2
                  , let prod = l1 * snd p2
                  -- For all pairs prod and p1
                  , (l2, l2Op) <- aStarSet constr prod (snd p1)
                  -- LEAPFROG-2 graph
                  --   (p1 + l1 node) + p2 * l1 + l2 node
                  , let depth = 3 + (minCostDepthOf lut $ snd p1) + (minCostDepthOf lut $ snd p2)
                  , let gp1 = unGraph $ lut Map.! fst p1 Map.! snd p1
                  , let gp2 = unGraph $ lut Map.! fst p2 Map.! snd p2
                  , let g' = Map.insert l1 (Node (Just l1Op) 0) gp1
                             `Map.union`
                             Map.insert l2 (Node (Just l2Op) 0) (multGraph l1 gp2)
                  , Map.notMember l1 gp1   && Map.notMember l1 gp2
                  , Map.notMember prod gp1 && Map.notMember prod gp2
                  , Map.notMember l2 gp1   && Map.notMember l2 gp2
                  , l2 <= maxF constr
                  --, minCostOf lut l2 >= cost || minCostDepthOf lut l2 >= depth
                  , minCostDepthOf lut l2 >= depth
                  ]
      addBetterGraph (coef, g) = Map.insertWith select coef g
      multGraph c = Map.map (\(Node aops d)->Node (fmap (multAop c) aops) d) .
                    Map.mapKeys (c*)
      multAop c (AOp cfg f1 f2) = AOp cfg (c*f1) (c*f2)
      addGraphs scmGs = foldr addBetterGraph scmGs newGraphs
      --TODO filter members
  in Map.adjust addGraphs cost lut

withCosts'
  :: Cost -> Cost -> Cost
  -> (SCMLookup -> (Cost, Fundamental) -> (Cost, Fundamental) -> Cost -> SCMLookup)
  -> SCMLookup
  -> SCMLookup
withCosts' c1 c2 cDest f lut =
  foldr (\(p1,p2) lut -> f lut p1 p2 cDest) lut
  [( (c1,p1), (c2,p2) )
  | p1 <- Map.keys $ lut Map.! c1
  , p2 <- Map.keys $ lut Map.! c2
  ]

withCosts c1 c2 cDest f = withCosts' c2 c1 cDest f . withCosts' c1 c2 cDest f

insertCost1s :: AOpConstr constr => constr -> GraphSelector -> SCMLookup -> SCMLookup
insertCost1s s c = withCosts' 0 0 1 (insertAdds s c)

insertCost2s :: AOpConstr constr => constr -> GraphSelector -> SCMLookup -> SCMLookup
insertCost2s s c = withCosts  1 1 2 (insertMults s c).
                   withCosts' 0 1 2 (insertAdds  s c)

insertCost3s :: AOpConstr constr => constr -> GraphSelector -> SCMLookup -> SCMLookup
insertCost3s s c = withCosts  1 2 3 (insertMults s c) .
                   withCosts' 1 1 3 (insertAdds  s c) .
                   withCosts' 0 2 3 (insertAdds  s c)

insertCost4s :: AOpConstr constr => constr -> GraphSelector -> SCMLookup -> SCMLookup
insertCost4s s c = withCosts  1 3 4 (insertMults s c) .
                   withCosts  2 2 4 (insertMults s c) .
                   withCosts' 1 2 4 (insertAdds  s c) .
                   withCosts' 0 3 4 (insertAdds  s c) .
                   withCosts  1 1 4 (insertLeapfrog2s s c)

insertCost5s :: AOpConstr constr => constr -> GraphSelector -> SCMLookup -> SCMLookup
insertCost5s s c = withCosts  1 4 5 (insertMults s c) .
                   withCosts  2 3 5 (insertMults s c) .
                   withCosts' 0 4 5 (insertAdds  s c) .
                   withCosts' 1 3 5 (insertAdds  s c) .
                   withCosts' 2 2 5 (insertAdds  s c) .
                   withCosts  1 2 5 (insertLeapfrog2s s c)
  -- TODO add leapfrog-3 (1,1,5)

costGraphs :: AOpConstr constr => constr -> [Metric] -> SCMLookup
costGraphs constr metrics =
  insertCost5s constr select .
  insertCost4s constr select .
  insertCost3s constr select .
  insertCost2s constr select .
  insertCost1s constr select $
  Map.fromList [(0, Map.singleton 1 (Graph $ Map.singleton 1 (Node Nothing 0)))
               ,(1, Map.empty)
               ,(2, Map.empty)
               ,(3, Map.empty)
               ,(4, Map.empty)
               ,(5, Map.empty)]
  where
  select = selectByMetrics metrics

lowCostGraphs :: AOpConstr constr => constr -> [Metric] -> SCMLookup
lowCostGraphs constr metrics =
  insertCost2s constr select .
  insertCost1s constr select $
  Map.fromList [(0, Map.singleton 1 (Graph $ Map.singleton 1 (Node Nothing 0)))
               ,(1, Map.empty)
               ,(2, Map.empty)
               ]
  where
  select = selectByMetrics metrics

saveLUT :: FilePath -> SCMLookup -> IO ()
saveLUT f lut = B.writeFile f . compress $ encode lut

loadLUT :: FilePath -> IO SCMLookup
loadLUT f = fmap (decode . decompress) $ B.readFile f
