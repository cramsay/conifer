{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, KindSignatures, TypeFamilies, UndecidableInstances #-}

module Filter.MultBlock where

--import Graph.Hcub
import Graph.Pipelined
import Graph.Aop
import Filter.THUtil
import Clash.Prelude

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Data.Singletons.Prelude (Apply, TyFun, type (@@))
import Data.Proxy
import Data.Kind (Type)

data AppendF (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (AppendF a) l = a -> Vec l a

toMCM :: forall dom gate sync a n m.
           (HiddenClockResetEnable dom, NFDataX a, Integral a, Bits a, KnownNat n, KnownNat m)
        => (SNat m, Vec (n+m) PNode) -> (Signal dom a -> Vec n (Signal dom a))
toMCM (d, ns) = \x -> drop d (fAllNodes x)
  where fAllNodes = dfold m step (\x->Nil) (reverse ns)
        m = (Proxy :: Proxy (AppendF (Signal dom a)))
        step :: SNat l -> PNode -> AppendF (Signal dom a) @@ l -> AppendF (Signal dom a) @@ (l + 1)
        step SNat n f = doPFunc n f

toPaddedMCM :: forall dom gate sync a n m d.
                 (HiddenClockResetEnable dom, NFDataX a, Integral a, Bits a, KnownNat n, KnownNat m
                 ,KnownNat d, Default a)
              => ((SNat m, Vec (n+m) PNode), SNat d) -> (Signal dom a -> Vec n (Signal dom a))
toPaddedMCM ((m,ns),d) x = toMCM (m, ns) . last $ iterate (addSNat d1 d) (register def) x

doPFunc :: forall dom gate sync a l.
           (HiddenClockResetEnable dom, NFDataX a, Integral a, Bits a, KnownNat l)
        => PNode -> (Signal dom a -> Vec l (Signal dom a))
        -> (Signal dom a -> Vec (l + 1) (Signal dom a))
doPFunc n f x = let ns = f x
                in ns :< nodeLogic n x ns
  where nodeLogic (PNodeAOp (AOp (AConf l1 l2 r s) p1 p2)) _ ns =
          let inp1 = liftA2 shiftL (ns !! p1) (pure l1)
              inp2 = liftA2 shiftL (ns !! p2) (pure l2)
              unshiftedOut = case s of
                POS -> inp1 + inp2
                NEG -> inp1 - inp2
              out = liftA2 shiftR unshiftedOut (pure r)
          in register 0 out
        nodeLogic (PNodeShift p s False) _ ns =
          liftA2 shiftL (ns!! p) (pure s)
        nodeLogic (PNodeShift p s True) _ ns =
          fmap negate $ liftA2 shiftL (ns!! p) (pure s)
        nodeLogic (PNodePipe p)    _ ns =
          register 0 $ ns !! p
        nodeLogic (PNodeIn ) _ ns = x
        nodeLogic (PNodeZero ) _ ns = 0
