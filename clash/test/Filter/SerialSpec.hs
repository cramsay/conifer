module Filter.SerialSpec (
   spec
  ,refFir
  ,refMCMCircuit
  )where

import Prelude
import Filter.Serial
import qualified Clash.Prelude as Clash
import Clash.Prelude (Signed, Signal, NFDataX, SNat(..), simulate, simulate_lazy, System, fromList, snatToNum, toList, Vec(..), delay, snatProxy)
import Test.QuickCheck hiding (sample)
import Test.Hspec
import Util
import Data.Reflection (reifyNat)

spec =  describe "Serial FIR filters" $ do
          specify "direct form impulse response"
            $ property $ prop_FirDirectImpResp
          specify "transpose form equivalence"
            $ property $ prop_FirTranspose
          specify "transpose with simple multiplier block equivalence"
            $ property $ prop_FirTransposeMCM

-- We're going to check equivalence between a simple direct form FIR and our
-- other versions... First we should ensure the direct form FIR is behaving.
-- Let's do that by checking it's impulse response
refFir coeffs inputs = drop 1 $ simulate @System (firDirect coeffs) (0 : inputs ++ repeat 0)

prop_FirDirectImpResp :: EVec 1 1 (Signed 19) -> Property
prop_FirDirectImpResp (EVec _ _ _ coeffs) = exp === got
  where
  exp = toList coeffs
  got
    = take (length coeffs)
    $ refFir coeffs (1 : repeat 0)

-- Let's check our transpose filter looks equivalent to our reference
prop_FirTranspose :: EVec 1 1 (Signed 19) -> [Signed 19] -> Property
prop_FirTranspose (EVec _ _ _ coeffs) inputs = exp === got
  where
  exp
    = take (length inputs)
    $ refFir coeffs inputs
  got
    = take (length inputs)
    . drop 2
    $ simulate @System (firTranspose coeffs)
                       (0 : inputs ++ repeat 0)

-- Let's check our transpose filter with a simple delayed multiplier block
refMCMCircuit n coeffs = \x -> Clash.map (\c-> regN n $ pure c * x) coeffs
  where
  regN n = Clash.last . Clash.iterate (Clash.addSNat Clash.d1 n) (Clash.register 0)

prop_FirTransposeMCM :: Positive Int -> EVec 1 1 (Signed 19) -> [Signed 19] -> Property
prop_FirTransposeMCM delay (EVec _ _ _ coeffs) inputs = exp === got
  where
  exp
    = take (length inputs)
    $ refFir coeffs inputs
  got
    = take (length inputs)
    . drop (2 + delay')
    $ reifyNat (toInteger delay')
    (\dp -> simulate @System (firTransposeMCM (refMCMCircuit (snatProxy dp) coeffs))
                             (0 : inputs ++ repeat 0)
    )
  delay' = getPositive delay
