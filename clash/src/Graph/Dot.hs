module Graph.Dot (
  showDot
  ,writeDotFile
  ,writeDotPDF
) where

import Prelude
import Text.Printf
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Process

import Graph.Aop
import Graph.SingleConstantMult
import Graph.Pipelined
import Graph.Util

header = unlines [
  "digraph multiplier_block {",
  "graph [pad=0.25, nodesep=0.25, ranksep=2];",
  "  rankdir=LR;"
  ]

footer = "}"

showFund n
  | n < 0 = "Neg" ++ show (negate n)
  | otherwise = show n

showSign POS = "+"
showSign NEG = "-"

showEdge :: Stage -> Fundamental -> Fundamental -> Int -> String
showEdge = \s c e i -> printf "node_%d_%s:o -> node_%d_%s:e%d;\n" (s-1) (showFund e) s (showFund c) i

showEdgeI :: Int -> Stage -> Fundamental -> Fundamental -> Int -> String
showEdgeI = \ind s c e i -> printf "node_%d_%s:o -> node_%d_%s_%d:e%d;\n" (s-1) (showFund e) s (showFund c) ind i

showDotNode :: ((Stage, Fundamental), PNode) -> String
showDotNode ((s,c), n) = case n of
  (PNodeAOp (AOp (AConf l1 l2 rshift sign) e1 e2)) ->
    let edges = [showEdge s c e1 1, showEdge s c e2 2]
        node  = printf (concat [
                  "node [shape = Mrecord,"
                , "label=\"{ {<e1> %s | {<e2> %s} } | { \\<\\<%d | \\<\\<%d} "
                , "| <o> %dx }\"] node_%d_%d;"
                ])
                (showSign $ POS)
                (showSign $ sign)
                (l1)
                (l2)
                c
                s
                c
    in unlines $ node : edges
  (PNodePipe e)    ->
    let node = printf (concat [
                  "node [shape = square,"
                , "label=<z<SUP>-1</SUP>>] node_%d_%d;"
                ])
                s
                c
    in node ++ "\n" ++ showEdge s c e 1
  (PNodeShift e shift neg)   ->
    let c' = if neg then negate e * (2^shift) else e * (2^shift)
        node = printf (concat [
                  "node [shape = parallelogram,"
                , "label=\"\\<\\<%d %s\"] node_%d_%s;"
                ])
                shift
                (if neg then " *(-1)" else "")
                s
                (showFund c')
    in node  ++ "\n" ++ printf "node_%d_%d:o -> node_%d_%s:e%d;\n" (s-1) e s (showFund c') (1::Int)
  (PNodeIn)        -> "node [shape = oval, label=x] node_0_1;"
  (PNodeZero)      -> printf "node [shape = oval, label=0] node_%d_0;" s

groupDepths (PG pg) = Map.elems $ Map.mapWithKey (\k cs -> map (\a->(k,a)) $ Map.keys cs) pg

setRanks = concat . map (\grp->"{ rank = same; " ++ concat (map (\(s,c)->printf "node_%d_%s; " s (showFund c)) grp) ++ " }\n")

showDotOutput :: Stage -> Fundamental -> Int -> String
showDotOutput s c i =
  let node = printf (concat [
                "node [shape = oval,"
              , "label=\"%dx\"] node_%d_%s_%d;"
              ])
              c
              (s+1)
              (showFund c)
              i
  in node ++ "\n" ++ showEdgeI i (s+1) c c 1

showDot :: [Fundamental] -> PGraph -> String
showDot cs g = concat
  [header
  ,unlines $ map showDotNode $ concat $ Map.elems $ Map.mapWithKey (\s cs -> Map.toList $ Map.mapKeys (\a->(s,a)) cs) $ unPG g
  ,setRanks $ groupDepths g
  ,unlines $ map (\(c,i) -> showDotOutput maxDepth c i) $ zip cs [0..]
  --,setRanks $ [zip (repeat $ maxDepth + 1) cs]
  ,footer
  ]
  where maxDepth = fst . Map.findMax $ unPG g

writeDotFile :: [Fundamental] -> PGraph -> FilePath -> IO ()
writeDotFile cs g f = writeFile f $ showDot cs g

writeDotPDF :: [Fundamental] -> PGraph -> FilePath -> IO ()
writeDotPDF cs g f = do
                     writeDotFile cs g f
                     runCommand $ "dot  -Tpdf -O " ++ f
                     return ()
