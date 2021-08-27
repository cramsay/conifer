module Filter.PolyphaseSpec (
  spec
  ) where

import Prelude
import Filter.Serial
import Filter.Polyphase
import Filter.FFA (listSplit, listFlatten)
import qualified Clash.Prelude as Clash
import Clash.Prelude (KnownNat, Signed, Signal, NFDataX, SNat(..), simulate, simulate_lazy, System, fromList, snatToNum, toList, Vec(..), delay, type(*), type(^), type(+), snatProxy)
import Test.QuickCheck hiding (sample)
import Test.Hspec
import Util
import Filter.SerialSpec hiding (spec)
import Data.Reflection (reifyNat)

spec = describe "Polyphase FIR filters" $ do
          describe "Standard direct forms" $ do
            specify "2x2 polyphase direct form equivalence"
              $ property $ prop_FirPoly Clash.d2
            specify "4x4 polyphase direct form equivalence"
              $ property $ prop_FirPoly Clash.d4
            specify "8x8 polyphase direct form equivalence"
              $ property $ prop_FirPoly Clash.d8
            specify "16x16 polyphase direct form equivalence"
              $ property $ prop_FirPoly Clash.d16
          describe "MCM tranpose forms" $ do
            specify "2x2 polyphase MCM equivalence"
              $ property $ prop_FirPolyMCM Clash.d2
            specify "4x4 polyphase MCM equivalence"
              $ property $ prop_FirPolyMCM Clash.d4
            specify "8x8 polyphase MCM equivalence"
              $ property $ prop_FirPolyMCM Clash.d8
            specify "16x16 polyphase MCM equivalence"
              $ property $ prop_FirPolyMCM Clash.d16

-- We'll need to reshape our inputs and add a dummy 1st sample
-- (reset does something odd during 1st cycle in the simulation)
padVecInput p inputs = (Clash.repeat 0) : (listSplit p $ inputs ++ repeat 0)

-- Let's check polyphase filters with direct form sub-filters against our
-- reference implementation
prop_FirPoly
  :: SNat (n+1)                   -- ^ Parallelisation factor, p
  -> EVec (n+1) (n+1) (Signed 19) -- ^ Coefficients (with length p*x+p, with a random x)
  -> [Signed 19]                  -- ^ Inputs
  -> Property
prop_FirPoly _ (EVec p _ _ coeffs) inputs = exp === got
  where
  exp
    = take (length inputs)
    $ refFir coeffs inputs
  got
    = take (length inputs) . listFlatten . drop (1 + ceiling (logBase 2 (snatToNum p)))
    $ simulate @System (polyphase p firDirect coeffs)
                       (padVecInput p inputs)

-- Now we'll look at the MCM version of our polyphase filter. Let's just supply
-- a simple multiplier block function with a random delay since we'll be checking our
-- graph MCM algorithms elsewhere
prop_FirPolyMCM
  :: SNat (n+1)                   -- ^ Parallelisation factor, p
  -> Positive Int                 -- ^ Delay cycles for MCM block
  -> EVec (n+1) (n+1) (Signed 19) -- ^ Coefficients (with length p*x+p, with random x)
  -> [Signed 19]                  -- ^ Inputs
  -> Property
prop_FirPolyMCM _ delay (EVec p _ _ coeffs) inputs = exp === got
  where
  exp
    = take (length inputs)
    $ refFir coeffs inputs
  got
    = take (length inputs) . listFlatten . drop (delay' + 2 + ceiling (logBase 2 (snatToNum p)))
    $ reifyNat (toInteger delay')
    (\dp -> simulate @System (polyphase_MCM p (refMCMCircuit (snatProxy dp) coeffs) )
                             (padVecInput p inputs)
    )
  delay' = getPositive delay
