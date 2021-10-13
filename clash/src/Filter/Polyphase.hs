module Filter.Polyphase (
   polyphase
  ,polyphase_MCM
  ) where

import Clash.Prelude
import Filter.Serial

recombFs :: (HiddenClockResetEnable dom, KnownNat n, NFDataX a, Num a)
         => Vec n (Vec n (Signal dom a) -> Vec n (Signal dom a))
recombFs = iterateI f id
  where
    f acf = zipWith ($) (delay 0 +>> (repeat id)) . flip rotateRightS d1 . acf

polyphase :: (HiddenClockResetEnable dom, KnownNat m, KnownNat n, NFDataX a, Default a, Num a)
          => SNat n
          -> (Vec (m) c -> (Signal dom a) -> (Signal dom a)) -- ^ FIR function
          -> Vec (n*m) c
          -> Signal dom (Vec n a) -> Signal dom (Vec n a)
polyphase n fir ws xs =
  let xsMat = map (replicate n) $ unbundle xs
      firs = map fir (transpose $ unconcat n ws)
      filtedMat = map (zipWith ($) firs) xsMat
      transpMat = zipWith ($) recombFs filtedMat
      ys = foldl (zipWith (+)) (repeat 0) transpMat
  in bundle ys

polyphase_MCM :: (HiddenClockResetEnable dom, KnownNat m, KnownNat n, NFDataX a, Default a, Num a)
          => SNat n
          -> (Signal dom a -> Vec (n*(1+m)) (Signal dom a)) -- ^ Mult block function for _all_ coefficients
          -> Signal dom (Vec n a) -> Signal dom (Vec n a)
polyphase_MCM n mcm xs =
  let filterPhase x = map (flip firTransposeMCM x)
                          (map const (transpose . unconcat n $ mcm x))
      filtedMat = map filterPhase $ unbundle xs
      transpMat = zipWith ($) recombFs filtedMat
      ys = foldl (zipWith (+)) (repeat 0) transpMat
  in bundle ys
