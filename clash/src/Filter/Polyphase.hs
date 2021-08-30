module Filter.Polyphase (
   polyphase
  ,polyphase_MCM
  ) where

import Clash.Prelude
import Filter.Serial

reorder :: forall i dom n a . (HiddenClockResetEnable dom, KnownNat n, NFDataX a, Num a)
         => SNat i -> Vec n (Signal dom a) -> Vec n (Signal dom a)
reorder i xs = smap delayFsts $ rotateRightS xs i
  where
    delayFsts :: forall j dom a . (HiddenClockResetEnable dom, NFDataX a, Num a)
              => SNat j -> Signal dom a -> Signal dom a
    delayFsts j a = case (i `compareSNat` j) of
      SNatLE -> a
      SNatGT -> delay 0 a

polyphase :: (HiddenClockResetEnable dom, KnownNat m, KnownNat n, NFDataX a, Default a, Num a)
          => SNat (n+1)
          -> (Vec (m) c -> (Signal dom a) -> (Signal dom a)) -- ^ FIR function
          -> Vec ((n+1)*m) c
          -> Signal dom (Vec (n+1) a) -> Signal dom (Vec (n+1) a)
polyphase n fir ws xs =
  let xsMat = map (replicate n) $ unbundle xs
      firs = map fir (transpose $ unconcat n ws)
      filtedMat = map (zipWith ($) firs) xsMat
      transpMat = smap reorder filtedMat
      ys = fold (zipWith (\a b -> register 0 $ a+b)) transpMat
  in bundle ys

polyphase_MCM :: (HiddenClockResetEnable dom, KnownNat m, KnownNat n, NFDataX a, Default a, Num a)
          => SNat (n+1)
          -> (Signal dom a -> Vec ((n+1)*(1+m)) (Signal dom a)) -- ^ Mult block function for _all_ coefficients
          -> Signal dom (Vec (n+1) a) -> Signal dom (Vec (n+1) a)
polyphase_MCM n mcm xs =
  let filterPhase x = map (flip firTransposeMCM x)
                          (map const (transpose . unconcat n $ mcm x))
      filtedMat = map filterPhase $ unbundle xs
      transpMat = smap reorder filtedMat
      ys = fold (zipWith (\a b -> register 0 $ a+b)) transpMat
  in bundle ys
