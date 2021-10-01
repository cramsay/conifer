{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Filter.FFA (
   genFFA
  ,genFFA_MCM
  ,listSplit
  ,listSplitI
  ,listFlatten
  ) where

import Clash.Prelude hiding (Exp)
import Test.QuickCheck
import qualified Prelude as P
import Data.Reflection (reifyNat)
import qualified Data.Singletons.Prelude as SP
import Data.Proxy
import Data.Kind (Type)
import Control.Monad
import Language.Haskell.TH hiding (Type)
import Filter.Serial
import Filter.MultBlock
import Filter.THUtil


-------------------------------------------------------------------------------
-- Helpers

vecSplit :: (KnownNat n, KnownNat m) => SNat m -> Vec (m*n) a -> Vec m (Vec n a)
vecSplit _ = transpose . unconcatI

listSplit :: (KnownNat l, Default a) => SNat l -> [a] -> [Vec l a]
listSplit l  = P.map (unfoldr l uncons) . P.iterate (P.drop (snatToNum l))
  where uncons [] = def
        uncons (x:xs) = (x,xs)

listSplitI :: (KnownNat l, Default a) => [a] -> [Vec l a]
listSplitI = withSNat listSplit

listFlatten :: Default a => [Vec l a] -> [a]
listFlatten = P.concat . P.map toList

unconcat' :: (KnownNat n, KnownNat m) => SNat n -> Vec (n*m) a -> Vec n (Vec m a)
unconcat' n = unconcatI

revBitOrderMap :: (KnownNat n, 1<=n) => SNat n -> Vec (2^n) (Unsigned n)
revBitOrderMap l = map (unpack . v2bv . reverse . bv2v) $ iterate (pow2SNat l) (+1) 0

revBitOrderPerm :: (KnownNat n, 1<=n) => SNat n -> Vec (2^n) a -> Vec (2^n) a
revBitOrderPerm l x = gather x (revBitOrderMap l)

hSwap :: KnownNat n => (Vec (2^n)     a -> Vec (2^n)     a)
                    ->  Vec (2^(n+1)) a -> Vec (2^(n+1)) a
hSwap f x = let halves = unconcat' d2 x
            in concat $ f (halves !! 1) :> halves !! 0 :> Nil

data TreeTriples  (a :: Type) (n :: Nat) (f :: SP.TyFun Nat Type) :: Type
type instance SP.Apply (TreeTriples a n) l = Vec (3^l*n) a

-- Split coeffs into their FFA decompositions
splitCoeffs :: forall l n a. (Num a, KnownNat n, KnownNat l, 1<=l, 1<=n) => SNat n -> SNat (2^l) -> Vec (2^l*n) a -> Vec (3^l) (Vec n a)
splitCoeffs n l h = unconcatI $
                    dtfold (Proxy :: Proxy (TreeTriples a n)) id
                      (\_ a b -> a ++ zipWith (+) a b ++ b)
                      (revBitOrderPerm (logBaseSNat d2 l) $ vecSplit l h)

splitCoeffsI :: forall l n a. (Num a, KnownNat n, KnownNat l, 1<=l, 1<=n) => SNat (2^l) -> Vec (2^l*n) a -> Vec (3^l) (Vec n a)
splitCoeffsI = withSNat splitCoeffs

-------------------------------------------------------------------------------
-- Recursive stages

fp_pre_rec ::
  ( HiddenClockResetEnable dom
  , Num a
  , KnownNat n
  , NFDataX a
  , Default a) =>
  (Vec (2^n) (Signal dom a) -> Vec (3^n) (Signal dom a)) ->
  Vec (2^(n+1)) (Signal dom a) ->
  Vec (3^(n+1)) (Signal dom a)
fp_pre_rec f x = let x' = map (register 0) x
                     evens = head $ vecSplit d2 x'
                     odds  = last $ vecSplit d2 x'
                     mids  = zipWith (+) evens odds
                     evens' = f evens
                     odds'  = f odds
                     mids'  = f mids
                 in evens' ++ mids' ++ odds'

fp_filt_rec ::
     ( HiddenClockResetEnable dom
     , KnownNat n
     , KnownNat m
     , Num a
     , NFDataX a
     , Default a) =>
     Vec (2*(1+n)) a ->
     (Vec (1+n) a -> Vec m (Signal dom a) -> Vec m (Signal dom a)) ->
     Vec (3*m) (Signal dom a) -> Vec (3*m) (Signal dom a)
fp_filt_rec coeffs f = let h0 = head $ vecSplit d2 coeffs
                           h1 = last $ vecSplit d2 coeffs
                           hplus = zipWith (+) h0 h1
                       in concat . map (uncurry f) . zip (h0 :> hplus :> h1 :> Nil) . unconcat' d3

fp_post_rec ::
     ( HiddenClockResetEnable dom
     , Num a
     , NFDataX a
     , Default a
     , KnownNat n) =>
     (Vec (2^n) (Signal dom a) -> Vec (2^n) (Signal dom a)) ->
     (Vec (3^n) (Signal dom a) -> Vec (2^n) (Signal dom a)) ->
     Vec (3^(n+1)) (Signal dom a) -> Vec (2^(n+1)) (Signal dom a)
{- Fully pipelined version
fp_post_rec swap_f rec_f xs =
  let xs' = map rec_f $ unconcat' d3 xs
      z = delayV $ xs' !! 2
      odds = zipWith3 (\x y z -> delay def (delay def (y - x) - z)) (xs' !! 0) (xs' !! 1) z
      swps  = swap_f z
      swps_d = zipWith ($) (delay def +>> repeat id) swps
      evens = delayV $ zipWith (+) (delayV (xs' !! 0)) swps_d
      delayV = map (delay def)
  in evens ++ odds -}
fp_post_rec swap_f rec_f xs =
   let xs' = map rec_f $ unconcat' d3 xs
       odds = zipWith3 (\x y z -> delay def (y - z - x)) (xs' !! 0) (xs' !! 1) (xs' !! 2)
       swps  = swap_f $ xs' !! 2
       swps_d = zipWith ($) (delay def +>> repeat id) swps
       evens = map (delay def) $ zipWith (+) (xs' !! 0) swps_d
   in evens ++ odds


--------------------------------------------------------------------------------
-- If we wanted to define our x2, x4, and x8 filters without template haskell,
-- this is what we'd do...

{-

fp_pre_2 = fp_pre_rec id
fp_filt_2 ws fir = fp_filt_rec ws (fir)
fp_swap_2 = id
fp_post_2 = fp_post_rec fp_swap_2 id

fp_fir_2 ws fir = revBitOrderPerm d1 . fp_post_2 . fp_filt_2 ws fir . fp_pre_2

fp_pre_4 = fp_pre_rec fp_pre_2
fp_filt_4 ws fir = fp_filt_rec ws (\ws -> fp_filt_2 ws fir)
fp_swap_4 = hSwap fp_swap_2
fp_post_4 = fp_post_rec fp_swap_4 fp_post_2

fp_fir_4 ws fir = revBitOrderPerm d2 . fp_post_4 . fp_filt_4 ws fir . fp_pre_4

fp_pre_8 = fp_pre_rec fp_pre_4
fp_filt_8 ws fir = fp_filt_rec ws (\ws -> fp_filt_4 ws fir)
fp_swap_8 = hSwap fp_swap_4
fp_post_8 = fp_post_rec fp_swap_8 fp_post_4

fp_fir_8 ws fir = revBitOrderPerm d3 . fp_post_8 . fp_filt_8 ws fir . fp_pre_8
-}

--------------------------------------------------------------------------------
-- Template Haskell FFA generators

-- This version leaves the whole filtering stage as a parameter. We
-- can use that to generate multiplier block versions given some weights.
genFFAGivenFirs :: SNat n -> Q Exp
genFFAGivenFirs sn = [| \f -> bundle . $(reorder n) . $(post n) . f . $(pre n) . unbundle |]
  where
  n = snatToInteger sn
  {- Fully pipelined
  pre  1 = [| map (delay def) |] -}
  pre  1 = [| id |]
  pre  n = [|fp_pre_rec $(pre (n `div` 2))|]
  swap 2 = [|id|]
  swap n = [|hSwap $(swap (n `div` 2))|]
  post 2 = [|fp_post_rec $(swap 2) id|]
  post n = [|fp_post_rec $(swap n) $(post (n `div` 2))|]
  reorder n = [|revBitOrderPerm $(promoteIntTH . ceiling . logBase 2 $ fromIntegral n)|]

-- Generate an FFA structure with `n`-parallelism.
-- `n` must be a power of two.
-- Result accepts a generic FIR function and a set of weights.
genFFA :: SNat n -> Q Exp
genFFA sn = [| \fir ws -> $(genFFAGivenFirs sn) ($(filt n) ws (\ws->map (fir ws))) |]
  where
  n = snatToInteger sn
  filt 2 = [|\ws fir -> fp_filt_rec ws fir|]
  filt n = [|\ws fir -> fp_filt_rec ws (\ws'-> $(filt (n `div` 2)) ws' fir)|]

-- -- TODO need to rewrite this generator for genFFA with SNat n
--genFFAs :: Int -> Q [Dec]
--genFFAs n = forM (P.takeWhile (<=n) $ P.iterate (*2) 2) mkFfaDec
--  where mkFfaDec ith = funD name [clause [] (normalB (genFFA ith)) []]
--            where name = mkName $ "ffa" P.++ show ith

-- Version of genFFA which accepts a TH function for generating multiplier
-- blocks instead of a normal FIR function. weights must be supplied inside
-- splice, since they will change our FIR structures during compilation.
genFFA_MCM f n ws = [| let firs xs = zipWith ($) (map firTransposeMCM multBs) xs
                           multBs = $(genMults f ws n)
                       in $(genFFAGivenFirs n) firs
                    |]
  where
    genMults f ws n = listExprs $
                      P.map f
                      (toList . map toList . splitCoeffsI n $ map (fromIntegral) ws)
