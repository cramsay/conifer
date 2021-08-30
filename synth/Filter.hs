import Clash.Prelude

import Filter.FFA
import Filter.Serial
import Filter.Polyphase
import Graph.MCM
import ExampleCoeffs

createDomain vSystem{vName="SystemNR", vResetPolarity=ActiveLow}

{-
firFFA_MCM_HP :: forall a dom
              .  (KnownNat a, HiddenClockResetEnable dom)
              => Signal dom (Vec 16 (Signed a))
              -> Signal dom (Vec 16 (Signed (a+16+CLog 2 4+ 4*(CLog 2 16))))
              -- 2 = parallelism, 16 = coefficient wordlength, 16 = number of coefficients
firFFA_MCM_HP xs = $(genFFA_MCM (mcmPipelinedDepthHwTH HcubShallow 3)
                                (SNat :: SNat 16)
                                (map fromIntegral coeffsLpls)
                    ) (fmap (map resize) xs)

firPolyMCM_HP :: (KnownNat a, HiddenClockResetEnable dom)
              => Signal dom (Vec 16 (Signed a))
              -> Signal dom (Vec 16 (Signed (a+16+CLog 2 4 + CLog 2 16)))
              -- 16 = parallelism, 16 = coeff wordlength, (4*16) = number of coefficients
firPolyMCM_HP xs = polyphase_MCM
                     (SNat :: SNat 16)
                     $(mcmPipelinedDepthHwTH HcubShallow 3 (toList $ map fromIntegral coeffsLpls))
                     (fmap (map resize) xs)
-}

firPolyDirect :: forall p c a n dom
              .  (KnownNat (1+p), KnownNat c, KnownNat a, KnownNat n, HiddenClockResetEnable dom, 1<=p)
              => Vec ((1+n)*(1+p)) (Signed c)
              -> Signal dom (Vec (1+p) (Signed a))
              -> Signal dom (Vec (1+p) (Signed (a+c + CLog 2 (1+n) + CLog 2 p)))
firPolyDirect coeffs xs = polyphase (SNat :: SNat (1+p)) firDirect (map resize coeffs) (fmap (map resize) xs)

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
                    (firPolyDirect @15 @16 @16 @3 @SystemNR coeffsLpls x)
                    c r (toEnable $ pure True)
