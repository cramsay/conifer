{-# LANGUAGE TemplateHaskell #-}
module Filter.THUtil where

import Clash.Prelude hiding (Exp)
import qualified Prelude as P
import Language.Haskell.TH hiding (Type)
import Graph.Pipelined (PNode)

-- Promote an integral to a type-level Nat
promoteIntTH :: Integral n => n -> ExpQ
promoteIntTH n
  | n<0       = error "Cannot promote negative int to SNat, dummy."
  | n==0      = [|d0|]
  | otherwise = [| addSNat d1 $(promoteIntTH $ n-1) |]

-- Compose a list of TH exprs into _one_ TH expr, representing a vector of the values
listExprs :: [ExpQ] -> ExpQ
listExprs []     = [| Nil |]
listExprs (a:as) = [| $a :> $(listExprs as) |]

-- Convert a list of MCM Nodes and the number of internal nodes to a Vector of nodes and a type-level Nat
flatPGraphToVecTH :: (Int, [PNode]) -> ExpQ
flatPGraphToVecTH (i, g) = [|( $(promoteIntTH i), $(listToVecTH g) )|]

-- Same as flatPGraphToVecTH but tag depth information too
flatPGraphToVecDepthTH :: ((Int, [PNode]), Int) -> ExpQ
flatPGraphToVecDepthTH ((i, g), d) = [| (( $(promoteIntTH i), $(listToVecTH g) ), $(promoteIntTH d) ) |]
