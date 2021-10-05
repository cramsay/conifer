{-# OPTIONS_GHC -fconstraint-solver-iterations=40 #-}

import Clash.Prelude

import Filter.FFA
import Filter.Serial
import Filter.Polyphase
import Graph.MCM
import ExampleCoeffs

createDomain vSystem{vName="SystemNR", vResetPolarity=ActiveLow}

{-
firPolyDirect :: forall p c a n dom
              .  (KnownNat (1+p), KnownNat c, KnownNat a, KnownNat n, HiddenClockResetEnable dom, 1<=p)
              => Vec ((1+n)*(1+p)) (Signed c)
              -> Signal dom (Vec (1+p) (Signed a))
              -> Signal dom (Vec (1+p) (Signed (a+c + CLog 2 (1+n) + CLog 2 p)))
firPolyDirect coeffs xs = polyphase (SNat :: SNat (1+p)) firDirect (map resize coeffs) (fmap (map resize) xs)

firPolyMCM_LP :: (KnownNat a, HiddenClockResetEnable dom)
              => Signal dom (Vec 16 (Signed a))
              -> Signal dom (Vec 16 (Signed (a+16+CLog 2 4 + CLog 2 16)))
              -- 16 = parallelism, 16 = coeff wordlength, (4*16) = number of coefficients
firPolyMCM_LP xs = polyphase_MCM
                     (SNat :: SNat 16)
                     $(mcmPipelinedDepthHwTH HcubShallow 3 (toList $ map fromIntegral coeffsLpls))
                     (fmap (map resize) xs)
-}

firFFA_MCM_LP :: forall a dom
              .  (KnownNat a, HiddenClockResetEnable dom)
              => Signal dom (Vec 4 (Signed a))
              -> Signal dom (Vec 4 (Signed (a+16+CLog 2 16+ 4*(CLog 2 4))))
              -- 4 = parallelism, 16 = coefficient wordlength, (16*4) = number of coefficients
firFFA_MCM_LP xs = $(genFFA_MCM (mcmPipelinedDepthHwTH HcubShallow 3)
                                (SNat :: SNat 4)
                                (map fromIntegral coeffsLpls)
                    ) (fmap (map resize) xs)

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "fir_dut"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "x" ]
    , t_output = PortName "y"
    }) #-}
topEntity c r x = exposeClockResetEnable
                    (firFFA_MCM_LP @16 @SystemNR x)
                    c r (toEnable $ pure True)
