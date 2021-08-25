module Filter.FreqResp where

import Prelude
import qualified Clash.Prelude as Clash
import Clash.Prelude (KnownNat, SFixed, SNat(..), BitVector, Vec(..),
                      d0,d1,d2,d3,d4,d5,d6,d7,
                      System, simulate_lazy, df, pureDF, seqDF,
                      bundle, unbundle,
                      unSF, snatToNum, pow2SNat, fLitR, at, resize, simulate, Signed, Signal, type(+))

import Graphics.Rendering.Chart.Easy hiding (at,(:>))
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Filter.FFA (listSplit, listFlatten)
import Filter.Serial
import ExampleCoeffs

refFir :: KnownNat n => Vec (1+n) (Signed 16) -> [Signed 16] -> [Signed 32]
refFir coeffs inputs = drop 1 $ simulate @System (firDirect (Clash.map resize coeffs)) ((0 :: Signed 32) : map resize inputs ++ repeat 0)

-- Trig functions with freq, phase, and amplitude params
genCos f p a = [cos theta * a | let dtheta = 2*pi*f, theta <- iterate (+dtheta) (-(fromIntegral p-1)*dtheta)]
genSin f p a = [sin theta * a | let dtheta = 2*pi*f, theta <- iterate (+dtheta) (-(fromIntegral p-1)*dtheta)]

-- Measure the amplitude response of a single frequency
-- n is number of samples before valid output, f is frequency, a is amplitude
measureHf fir n f a = let sim gen = fromIntegral ((fir (map round $ gen f n a)) !! (n-1) ) / a :: Double
                      in (sim genCos, sim genSin)

-- Measure frequency response
measureH fir n nfreq a = take nfreq
                         [ measureHf fir n f a
                         | let df = 0.5/(fromIntegral nfreq)
                         , f <- iterate (+df) 0
                         ]

-- Example use
-- Ensure that the FIR can support inputs of +/- amplitude without saturating/wrapping!
-- e.g. main (refFir coeffsLpls) 64 (fromIntegral (maxBound :: Signed 16))
main fir n amp = toFile def "/tmp/freq_resps.png" $ do
    layout_title .= "Frequency responses"
    setColors [opaque blue, opaque red, opaque green]
    plot (line "fir" [zip freq h])
  where
  nfreq = 64*16
  h = toDb $ measureH fir n nfreq amp
  toDb = map (\x->logBase 10 x * 20) . iqAbs
  iqAbs = map (\(i,q)->sqrt $ i^2 + q^2)
  freq = take nfreq $ iterate (+(0.5/(fromIntegral nfreq))) 0 :: [Double]
