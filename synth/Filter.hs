import Clash.Prelude

import Filter.FFA
import Filter.Serial
import Filter.Polyphase
import Graph.MCM
import ExampleCoeffs

createDomain vSystem{vName="SystemNR", vResetPolarity=ActiveLow}

firFFA_MCM_HP :: forall a dom
              .  (KnownNat a, HiddenClockResetEnable dom)
              => Signal dom (Vec 2 (Signed a))
              -> Signal dom (Vec 2 (Signed (a+16+CLog 2 16+ 4*(CLog 2 2))))
              -- 2 = parallelism, 16 = coefficient wordlength, 16 = number of coefficients
firFFA_MCM_HP xs = $(genFFA_MCM (mcmPipelinedDepthHwTH HcubShallow 3)
                                (SNat :: SNat 2)
                                (map fromIntegral coeffsHpls)
                    ) (fmap (map resize) xs)

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "filter"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "x" ]
    , t_output = PortName "y"
    }) #-}
topEntity c r x = exposeClockResetEnable
                    (firFFA_MCM_HP @16 @SystemNR x)
                    c r (toEnable $ pure True)
