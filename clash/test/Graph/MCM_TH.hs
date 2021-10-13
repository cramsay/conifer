module Graph.MCM_TH (
   prop_MCM_HW_TH
  ,prop_MCM_Depth_HW_TH
) where

import Prelude
import Filter.Serial
import Filter.FFA
import qualified Clash.Prelude as Clash
import Clash.Prelude (KnownNat, Signed, Signal, NFDataX, SNat(..), simulate, simulate_lazy, System, fromList, snatToNum, toList, Vec(..), delay, type(*), type(^), type(+), type(<=), snatProxy, def, mulSNat, listToVecTH, bundle, resize)
import Test.QuickCheck hiding (sample, resize)
import Test.QuickCheck.Monadic (assert, monadicIO, PropertyM, run)
import Util
import Filter.Serial hiding (tests)
import Data.Reflection (reifyNat)
import Language.Haskell.TH hiding (Type)

import Graph.MCM
import Graph.Util

-- Generate tests for an MCM circuit given compile-time coefficients
--
-- We've got to actually account for wordlength growth here. We can't rely on
-- Signed's overflow behaving how we expect since we don't know what
-- intermediate arithmetic stages will be generated
prop_MCM_HW_TH :: McmAlgorithm
               -> [Signed 16] -- ^ Coefficients
               -> Q Exp
prop_MCM_HW_TH algo coeffs = [|
    let prop :: [Signed 16] -> Property
        prop inputs = exp === got $depthQ
          where
          inputs' = map resize inputs :: [Signed (16+16)]
          exp
            = map (\x -> map (\c -> x * resize c) coeffs) inputs'
          got d
            = map toList . take (length inputs') . drop d
            $ simulate @System ( bundle . $(mcmPipelinedHwTH algo (map fromIntegral coeffs)) ) (inputs' ++ repeat 0)
    in \i -> prop (i `asTypeOf` [def :: Signed 16])
  |]
  where
    depthQ = do expr <- runIO . fmap ((\i->[|i|]) . depthMetric)
                      $ mcmCombinatorial algo (map fromIntegral coeffs)
                expr

-- Generate tests for an MCM circuit with a minimum depth/latency given compile-time coefficients
prop_MCM_Depth_HW_TH :: Int -- ^ Minimum latency
               -> McmAlgorithm
               -> [Signed 16] -- ^ Coefficients
               -> Q Exp
prop_MCM_Depth_HW_TH depth algo coeffs = [|
    let prop :: [Signed 16] -> Property
        prop inputs = exp === got $depthQ
          where
          inputs' = map resize inputs :: [Signed (16+16)]
          exp
            = map (\x -> map (\c -> x * resize c) coeffs) inputs'
          got d
            = map toList . take (length inputs') . drop d
            $ simulate @System ( bundle . $(mcmPipelinedDepthHwTH algo depth (map fromIntegral coeffs)) ) (inputs' ++ repeat 0)
    in \i -> prop (i `asTypeOf` [def :: Signed 16])
  |]
  where
    depthQ = do expr <- runIO . fmap ((\i->[|i|]) . depthMetric)
                      $ mcmCombinatorial algo (map fromIntegral coeffs)
                expr

