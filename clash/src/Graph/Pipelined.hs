{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}


module Graph.Pipelined (
  Stage
  ,PNode(..)
  ,PGraph(..)
  ,pipeline
  ,flattenGraph
  ,getPGraphUsageEst
  ) where

import Prelude

import Graph.Aop
import Graph.SingleConstantMult
import Graph.Util
import GHC.Generics (Generic)
import Clash.Prelude (Lift)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe (fromMaybe)
import Data.List (findIndices, nub)

type Stage = Int
data PNode = PNodeAOp     AOp
             | PNodePipe  Fundamental
             | PNodeShift Fundamental Shift Bool --Bool is optional negation
             | PNodeIn
             | PNodeZero
  deriving (Show, Eq, Lift)

newtype PGraph = PG {unPG :: Map Stage (Map Fundamental PNode)}
  deriving (Show, Eq, Lift)


fundamentalToIndex :: [(Fundamental, PNode)] -> (Fundamental, PNode) -> (Fundamental, PNode)
fundamentalToIndex lst (f,node) = case node of
  PNodeAOp (AOp conf u v) -> (f, PNodeAOp $ AOp conf (indexOf u) (indexOf v))
  PNodePipe  u            -> (f, PNodePipe           (indexOf u)            )
  PNodeShift u s n        -> (f, PNodeShift          (indexOf u)  s n       )
  PNodeIn                 -> (f, PNodeIn                                    )
  PNodeZero               -> (f, PNodeZero                                  )
  where
    indexOf n = last $
                findIndices ((n==) . fst) lst

-- TODO we don't need to pass ws in here...
flattenGraph :: [Fundamental] -> PGraph -> (Int, [PNode])
flattenGraph ws (PG pg) =
  let maxStage = fst $ Map.findMax pg
      addStage lst stage = lst ++ ( map (fundamentalToIndex lst) $ Map.toList stage )
      flatNodes = Map.foldl addStage [] pg
      dropN = length flatNodes - length (nub ws)
      flatNodesInternal = take dropN flatNodes
      flatNodesExternalOrdered = map (\w-> fundamentalToIndex flatNodesInternal (w, pg Map.! maxStage Map.! w)) ws

  in (dropN, map snd $ flatNodesInternal ++ flatNodesExternalOrdered)

pipeline :: [Fundamental] -> Graph -> PGraph
pipeline ws (Graph g) =
  let ws'            = divEvens $ map abs ws
      flatAdders     = Map.map nodeToPNode g
      maxStage       = depthMetric $ Graph g
      emptyStages    = Map.fromList [(s,Map.empty) | s<-[0..maxStage+1]] -- Extra final stage for output shifting
      adders         = PG . Map.foldrWithKey
                       (\k node pg -> Map.adjust (Map.insert k node) (depthOfFundamental k $ Graph g) pg )
                       emptyStages $ Map.map nodeToPNode g
      pipelined      = foldr addRegistersForFundamental adders $ map (\a->(maxStage,a)) ws'
      pipedWithEvens = foldr shiftForEvenFundamentals pipelined $ map (\a->(maxStage+1,a)) ws
  in  pipedWithEvens

nodeToPNode :: Node -> PNode
nodeToPNode (Node mAop _) = fromMaybe PNodeIn $ fmap PNodeAOp mAop

addRegistersForFundamental :: (Stage, Fundamental) -> PGraph -> PGraph
addRegistersForFundamental (0,_) pg = pg
addRegistersForFundamental (s,c) pg = case (unPG pg) Map.! s Map.!? c of
  -- Enter pipe into graph and continue with stage-1
  Nothing -> PG . Map.adjust (Map.insert c (PNodePipe c)) s . unPG $ addRegistersForFundamental (s-1,c) pg
  -- This node exists, so just recurse
  Just edge -> case edge of
    (PNodeAOp (AOp cfg e1 e2)) -> addRegistersForFundamental (s-1, e1) $ addRegistersForFundamental (s-1, e2) pg
    (PNodePipe e)              ->                                        addRegistersForFundamental (s-1, e ) pg
    (PNodeShift e _ _)         ->                                        addRegistersForFundamental (s-1, e ) pg
    (PNodeIn)                  ->                                                                             pg
    (PNodeZero)                ->                                                                             pg

shiftForEvenFundamentals :: (Stage, Fundamental) -> PGraph -> PGraph
shiftForEvenFundamentals (s,0) pg = PG . Map.adjust (Map.insert 0 (PNodeZero )) s $ unPG pg
shiftForEvenFundamentals (s,c) pg =
  let shift = floor . logBase 2 . fromIntegral $ (abs c) `div` divEven (abs c)
  in  PG . Map.adjust (Map.insert c (PNodeShift (divEven $ abs c) shift (c<0))) s $ unPG pg

-- TODO Find a way to encode the order of original coefficients here... we're
-- passing around [Fundamental] -> PGraph -> ...all over the place here. Need to merge
-- the two bits of data.


getBits :: Fundamental -> Int
getBits = ceiling . logBase 2 . fromIntegral

getLUTPNode :: (Fundamental,PNode) -> Int
getLUTPNode (f, node) = case node of
  PNodeAOp _ -> getBits f
  otherwise  -> 0

getFFPNode :: (Fundamental,PNode) -> Int
getFFPNode (f, node) = case node of
  PNodeAOp  _ -> getBits f
  PNodePipe _ -> getBits f
  otherwise  -> 0

getPGraphUsageEst :: PGraph -> Int
getPGraphUsageEst (PG g) = let lst = concat . map Map.toList $ Map.elems g
                               luts = sum $ map getLUTPNode lst
                               ffs  = sum $ map getFFPNode  lst
                           in max luts ffs
