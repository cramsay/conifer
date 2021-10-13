{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Util where

import Prelude
import Clash.Prelude (Vec(..), SNat(..), listToVecTH, KnownNat, System, fromList, sample,withSNat, snatProxy, type(+), type(*), type(^), addSNat, mulSNat, powSNat, lengthS)
import Test.QuickCheck
import Data.Reflection (reifyNat)

-- Existentially sized vector
-- The size is generated by quickCheck then augmented with a scaling and an
-- offset so you can keep most constraints happy.
data EVec s o e where
  EVec :: (KnownNat x, KnownNat s, KnownNat o) => SNat s -> SNat o -> SNat x -> Vec (x*s+o) e -> EVec s o e

deriving instance Show e => Show (EVec s o e)

instance forall e s o . (Arbitrary e, KnownNat s, KnownNat o) => Arbitrary (EVec s o e) where
  arbitrary = sized $ \size ->
              do n <- choose (0, fromIntegral size)
                 reifyNat n (\x -> withSNat (EVec s o) <$>
                              genVec (addSNat (mulSNat (snatProxy x) (SNat :: SNat s))
                                      (SNat :: SNat o) ))
    where
    s = SNat :: SNat s
    o = SNat :: SNat o
    genVec :: (KnownNat n, Arbitrary a) => SNat n -> Gen (Vec n a)
    genVec SNat = arbitrary

-- Type for valid parallelisation factors (type level version of 2^(n+1))
-- Our quickCheck implementation will only generate up to 16 to keep test
-- run-time managable
data EParallel where
  EParallel :: KnownNat n => SNat (2^n) -> EParallel

deriving instance Show EParallel

instance Arbitrary EParallel where
  arbitrary = do n <- elements [1,2,3,4]
                 pure $ reifyNat n (\x -> EParallel $ powSNat (SNat :: SNat 2) (snatProxy x))
