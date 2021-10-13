{-# Language DeriveLift #-}
module Graph.MCM where

import Prelude
import Data.Maybe
import Control.Monad.Reader
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Filter.THUtil
import Filter.MultBlock

import Graph.Util
import Graph.SingleConstantMult
import Graph.Aop
import Graph.Pipelined

import Graph.RAGn
import Graph.RSG
import Graph.Hcub
import Graph.Dot

import System.IO

data McmAlgorithm = RSG | RAGn | Hcub | HcubShallow
  deriving (Show, Eq, Lift)

getFn :: McmAlgorithm -> Reader (McmConfig AOdd) Graph
getFn RSG         = rsg
getFn RAGn        = ragn
getFn Hcub        = hcub
getFn HcubShallow = hcub

getCfg :: McmAlgorithm -> ([Fundamental] -> IO(McmConfig AOdd))
getCfg RSG         = rsgConfig
getCfg RAGn        = ragnConfig
getCfg Hcub        = hcubConfig
getCfg HcubShallow = hcubShallowConfig

-- TODO Then implement state monad for distance sharing
-- TODO Pass maxDepth around via Reader McmConfig to distance estimates

mcmCombinatorial :: McmAlgorithm -> [Fundamental] -> IO Graph
mcmCombinatorial algo ws = do
  cfg <- getCfg algo (map abs ws)
  let g = runReader (getFn algo) cfg

  hPutStrLn stdout $ "Adder Count = " ++ (show $ adderCostMetric g) -- Using System.IO to ensure it gets printed even when run via template haskell
  hPutStrLn stdout $ "Depth = "      ++ (show $ depthMetric g)
  hFlush stdout
  return g

mcmPipelined :: McmAlgorithm -> [Fundamental] -> IO PGraph
mcmPipelined algo ws = fmap (pipeline ws) $ mcmCombinatorial algo ws

mcmPipelinedTH :: McmAlgorithm -> [Fundamental] -> ExpQ
mcmPipelinedTH algo ws = do
  expr <- runIO . fmap (flatPGraphToVecTH . flattenGraph ws . pipeline ws . avoidAbs) $ mcmCombinatorial algo ws
  expr

mcmPipelinedDepthTH :: McmAlgorithm -> Int -> [Fundamental] -> ExpQ
mcmPipelinedDepthTH algo depth ws = do
  expr <- runIO . fmap (\g -> flatPGraphToVecDepthTH (flattenGraph ws . pipeline ws $ avoidAbs g, depth - depthMetric g) ) $ mcmCombinatorial algo ws
  expr

mcmPipelinedHwTH :: McmAlgorithm -> [Fundamental] -> ExpQ
mcmPipelinedHwTH algo ws = [| toMCM $(mcmPipelinedTH algo ws) |]

mcmPipelinedDepthHwTH :: McmAlgorithm -> Int -> [Fundamental] -> ExpQ
mcmPipelinedDepthHwTH algo depth ws = [| toPaddedMCM $(mcmPipelinedDepthTH algo depth ws) |]
