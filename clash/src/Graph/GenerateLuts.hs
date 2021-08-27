module Graph.GenerateLuts where

import Graph.SingleConstantMult
import Graph.Util
import Graph.Aop

main = do
  let lutMinCost = costGraphs (AOdd 20) [verticeSumMetric]
  let lutMinDepth = costGraphs (AOdd 20) [depthMetric, verticeSumMetric]
  saveLUT "LutMinCost.bin"  lutMinCost
  saveLUT "LutMinDepth.bin" lutMinDepth
