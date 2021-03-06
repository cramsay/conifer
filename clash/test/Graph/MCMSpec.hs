module Graph.MCMSpec (
  spec
  ) where

import Clash.Prelude (toList)
import Graph.MCM
import Graph.Aop
import Graph.Pipelined
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, PropertyM, run)
import Test.Hspec
import Graph.MCM_TH
import ExampleCoeffs

import Prelude
import Data.Map.Strict hiding (map, toList)

spec =  describe "MCM Algorithms" $ do
          describe "Graph construction" $ do
            specify "Graphs from RSG sync & arithmetic"
              $ property $ prop_MCMGraph RSG
            specify "Graphs from RAGn sync & arithmetic"
              $ property $ prop_MCMGraph RAGn
            specify "Graphs from Hcub sync & arithmetic"
              $ property $ prop_MCMGraph Hcub
            specify "Graphs from HcubShallow sync & arithmetic"
              $ property $ prop_MCMGraph HcubShallow
          describe "Circuit construction examples" $ do
            specify "RSG circuit for low-pass coeffs"
              $ property $(prop_MCM_HW_TH RSG (toList coeffsLpls))
            specify "RSG circuit for high-pass coeffs"
              $ property $(prop_MCM_HW_TH RSG (toList coeffsHpls))
            specify "RSG circuit for band-pass coeffs"
              $ property $(prop_MCM_HW_TH RSG (toList coeffsBpls))
            specify "RSG circuit for low-pass coeffs"
              $ property $(prop_MCM_HW_TH RAGn (toList coeffsLpls))
            specify "RAGn circuit for high-pass coeffs"
              $ property $(prop_MCM_HW_TH RAGn (toList coeffsHpls))
            specify "RAGn circuit for band-pass coeffs"
              $ property $(prop_MCM_HW_TH RAGn (toList coeffsBpls))
            specify "RAGn circuit for low-pass coeffs"
              $ property $(prop_MCM_HW_TH Hcub (toList coeffsLpls))
            specify "Hcub circuit for high-pass coeffs"
              $ property $(prop_MCM_HW_TH Hcub (toList coeffsHpls))
            specify "Hcub circuit for band-pass coeffs"
              $ property $(prop_MCM_HW_TH Hcub (toList coeffsBpls))
            specify "HcubShallow circuit for low-pass coeffs"
              $ property $(prop_MCM_HW_TH HcubShallow (toList coeffsLpls))
            specify "HcubShallow circuit for high-pass coeffs"
              $ property $(prop_MCM_HW_TH HcubShallow (toList coeffsHpls))
            specify "HcubShallow circuit for band-pass coeffs"
              $ property $(prop_MCM_HW_TH HcubShallow (toList coeffsBpls))


instance Arbitrary McmAlgorithm where
  arbitrary = elements [RSG, RAGn, Hcub, HcubShallow]

-- Does a given MCM algorithm actually describe the multiplications we ask for?
prop_MCMGraph :: McmAlgorithm -> [Fundamental] -> Property
prop_MCMGraph algo ws = monadicIO prop
  where prop = do g <- run $ mcmPipelined algo ws
                  checkPGraph ws g

-- Check that the last stage of a pipelined graph contains all coefficients
checkPGraph :: [Fundamental] -> PGraph -> PropertyM IO ()
checkPGraph ws pgraph = assert . and $ map (\w -> calcCoeff pgraph s w == w) ws
  where s = fst . findMax $ unPG pgraph

-- Calculate the coefficient generated by a specific node in a pipelined graph
calcCoeff :: PGraph -> Stage -> Fundamental -> Fundamental
calcCoeff pgraph s w
  = let stage = unPG pgraph ! s
        node  = stage ! w
    in case node of
         PNodeAOp  (AOp conf u v) -> aOp conf (calcCoeff pgraph (s-1) u) (calcCoeff pgraph (s-1) v)
         PNodePipe  n             -> calcCoeff pgraph (s-1) n
         PNodeShift n shft True   -> negate $ calcCoeff pgraph (s-1) n * (2^shft)
         PNodeShift n shft False  ->          calcCoeff pgraph (s-1) n * (2^shft)
         PNodeIn                  -> 1
         PNodeZero                -> 0

