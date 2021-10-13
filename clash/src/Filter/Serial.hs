{-# LANGUAGE NoImplicitPrelude #-}

module Filter.Serial (
   firTransposeMCM
  ,firTranspose
  ,firDirect
  ) where

import Clash.Prelude

-- FIR in transpose form with multiplier block
firTransposeMCM
  :: (HiddenClockResetEnable dom, KnownNat n, Num a, NFDataX a)
  => (Signal dom a -> Vec (1+n) (Signal dom a)) -- ^ mult block function
  -> Signal dom a                                        -- ^ input
  -> Signal dom a                                        -- ^ output
firTransposeMCM multB x = foldl f 0 (reverse $ multB x)
  where f ac x = register 0 $ ac + x

-- FIR in transpose form with standard multiplications
firTranspose
  :: (HiddenClockResetEnable dom, KnownNat n, Num a, NFDataX a)
  => Vec (1+n) a  -- ^ Coeffs
  -> Signal dom a -- ^ input
  -> Signal dom a -- ^ output
firTranspose ws = firTransposeMCM (\y-> map (\c-> (pure c)*y) ws)

-- FIR in direct form
firDirect
  :: (HiddenClockResetEnable dom, KnownNat n, Num a, NFDataX a, Default a)
  => Vec (1+n) a  -- ^ Coeffs
  -> Signal dom a -- ^ input
  -> Signal dom a -- ^ output
firDirect ws x = dotp (map pure ws) (window x)
  where
    dotp as bs = foldl1 f (zipWith (*) as bs)
    f ac x = ac + x
