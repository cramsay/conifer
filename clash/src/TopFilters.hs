module TopFilters where

import Clash.Prelude
import Filter.FFA
import Filter.Serial
import Filter.Polyphase
import Filter.MultBlock
import Graph.MCM

n = d4
coeffs = iterate d32 (+1) (1::Signed 16)
inputs = toList . unconcat n $ (1::Signed 16) :> (replicate d63 0)

--myfir_ffa = $(genFFA d4) firTranspose coeffs
--myfir_ffa_sim = simulateN @System 16 myfir_ffa inputs
--
--myfir_mcm = $(genFFA_MCM (mcmPipelinedDepthHwTH HcubShallow 3) d4 (iterate d32 (+1) (1::Signed 16)))
-- -- where mcm ws = [| toPaddedMCM $(mcmPipelinedDepthTH HcubShallow 3 ws) |]
--myfir_mcm_sim = simulateN @System 16 myfir_mcm inputs


{-
myfir_poly = polyphase n firTranspose coeffs
myfir_poly_sim = simulateN @System 16 myfir_poly inputs

myfir_poly_mcm = polyphase_MCM n $ toMCM mcm
  where mcm = $(mcmPipelinedTH HcubShallow (toList $ iterate d32 (+1) 1) )
myfir_poly_mcm_sim = simulateN @System 16 myfir_poly_mcm inputs

createDomain vSystem{vName="SystemNR", vResetPolarity=ActiveLow}

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "fir_dut"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "x" ]
    , t_output = PortName "y"
    }) #-}

topEntity
  :: Clock  SystemNR
  -> Reset  SystemNR
  -> Signal SystemNR (Vec 4 (Signed 16))
  -> Signal SystemNR (Vec 4 (Signed 16)) -- (Unsigned (16+12+CLog 2 10)))
topEntity c r x = exposeClockResetEnable (myfir_mcm x) c r (toEnable $ pure True)
-}
