module Filter.FFASpec (
  spec
  ) where

import Prelude
import Filter.Serial
import Filter.FFA
import qualified Clash.Prelude as Clash
import Clash.Prelude (KnownNat, Signed, Signal, NFDataX, SNat(..), simulate, simulate_lazy, System, fromList, snatToNum, toList, Vec(..), delay, type(*), type(^), type(+), snatProxy, resize)
import Test.QuickCheck hiding (sample, resize)
import Test.Hspec
import Util
import ExampleCoeffs
import Filter.SerialSpec hiding (spec)
import Filter.FFA_TH
import Data.Reflection (reifyNat)

spec = describe "Fast FIR algorithm filters" $ do
          describe "FFA with MCM-based subfilters" $ do
            specify "Low-pass 2x2 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d2 Clash.d3 $ Clash.map resize coeffsLpls)
            specify "High-pass 2x2 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d2 Clash.d3 $ Clash.map resize coeffsHpls)
            specify "Band-pass 2x2 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d2 Clash.d3 $ Clash.map resize coeffsBpls)
            specify "Low-pass 4x4 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d4 Clash.d3 $ Clash.map resize coeffsLpls)
            specify "High-pass 4x4 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d4 Clash.d3 $ Clash.map resize coeffsHpls)
            specify "Band-pass 4x4 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d4 Clash.d3 $ Clash.map resize coeffsBpls)
            specify "Low-pass 8x8 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d8 Clash.d3 $ Clash.map resize coeffsLpls)
            specify "High-pass 8x8 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d8 Clash.d3 $ Clash.map resize coeffsHpls)
            specify "Band-pass 8x8 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d8 Clash.d3 $ Clash.map resize coeffsBpls)
            specify "Low-pass 16x16 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d16 Clash.d3 $ Clash.map resize coeffsLpls)
            specify "High-pass 16x16 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d16 Clash.d3 $ Clash.map resize coeffsHpls)
            specify "Band-pass 16x16 FFA MCM equivalence"
              $ property $ $(prop_FirFFA_MCM_TH Clash.d16 Clash.d3 $ Clash.map resize coeffsBpls)
          describe "FFA with direct form subfilters" $ do
            specify "2x2 FFA direct form equivalence"
              $ property $ $(prop_FirFFA_TH Clash.d2 Clash.d64)
            specify "4x4 FFA direct form equivalence"
              $ property $ $(prop_FirFFA_TH Clash.d4 Clash.d64)
            specify "8x8 FFA direct form equivalence"
              $ property $ $(prop_FirFFA_TH Clash.d8 Clash.d32)
            specify "16x16 FFA direct form equivalence"
              $ property $ $(prop_FirFFA_TH Clash.d16 Clash.d32)

-- See Test.Filter.FFA_TH for bodies... not included here due to TH stage
-- restrictions
