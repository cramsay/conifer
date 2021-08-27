module Filter.FFA_TH (
   prop_FirFFA_TH
  ,prop_FirFFA_MCM_TH
  ) where

import Prelude
import Filter.Serial
import Filter.FFA
import qualified Clash.Prelude as Clash
import Clash.Prelude (KnownNat, Signed, Signal, NFDataX, SNat(..), simulate, simulate_lazy, System, fromList, snatToNum, toList, Vec(..), delay, type(*), type(^), type(+), type(<=), snatProxy, def, mulSNat, listToVecTH)
import Test.QuickCheck hiding (sample)
import Util
import Filter.Serial hiding (tests)
import Data.Reflection (reifyNat)
import Language.Haskell.TH hiding (Type)

-- We'll need to reshape our inputs and add a dummy 1st sample
-- (reset does something odd during 1st cycle in the simulation)
padVecInput p inputs = (Clash.repeat 0) : (listSplit p $ inputs ++ repeat 0)

-- Generate tests for our FFA structures with direct form FIR subfilters against
-- the reference

-- Note: we can't parameterise the parallelisation factor or coefficients as
-- nicely as our polyphase filters due to our use of template Haskell... We need
-- to know these parameters at compile time, and quickCheck would only supply
-- them at run time.
--
-- Here we generate tests for a configurable but constant number of coefficients
-- and parallelisations.
prop_FirFFA_TH :: (KnownNat n, KnownNat m)
               => SNat (2^(n+1))       -- ^ Parallelisation factor, p
               -> SNat (2^(n+1)*(m+1)) -- ^ Total number of coefficients
               -> Q Exp
prop_FirFFA_TH p n = [|
    let prop coeffs inputs = exp === got
          where
          exp
            = take (length inputs)
            $ refFir coeffs inputs
          got
            = take (length inputs) . listFlatten . drop (1 + 2 * (round (logBase 2 (snatToNum p))))
            $ simulate @System ($(genFFA p) firDirect coeffs)
                               (padVecInput p inputs)
    in \c i -> prop (c `asTypeOf` Clash.replicate n (def :: Signed 19))
                    (i `asTypeOf` [def :: Signed 19])
  |]

-- Now for our FFA with MCM blocks. Similar restrictions but we also need to
-- know the _values_ of the coefficients at compile time, since this can affect
-- the structure of the MCM blocks.
prop_FirFFA_MCM_TH :: (KnownNat n, KnownNat m, 1<=m)
               => SNat (2^(n+1))              -- ^ Parallelisation factor
               -> SNat d                      -- ^ MCM delay
               -> Vec (2^(n+1)*m) (Signed 19) -- ^ Coefficients
               -> Q Exp
prop_FirFFA_MCM_TH p d coeffs = [|
    let prop :: [Signed 19] -> Property
        prop inputs = exp === got
          where
          exp
            = take (length inputs)
            $ refFir coeffs inputs
          got
            = take (length inputs) . listFlatten . drop (2 + snatToNum d + 2 * (round (logBase 2 (snatToNum p))))
            $ simulate @System
              $(let mcm ws = [| refMCMCircuit d $(listToVecTH (ws :: [Signed 19])) |]
                in genFFA_MCM mcm p coeffs)
              (padVecInput p inputs)
    in \i -> prop (i `asTypeOf` [def :: Signed 19])
  |]
