{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Graph.Aop where

import Prelude

import Data.Bits (shiftL, shiftR, countTrailingZeros)
import GHC.Generics (Generic)
import Data.List (nub, transpose, concat)
import Clash.Prelude (Lift)
--import Data.Binary
--import Data.Maybe


-- NOTE: Consider making Fundamental strictly positive (and odd?)
type Fundamental = Int
type Shift = Int
data Sign = POS | NEG
  deriving (Eq, Show, Generic, Lift)

-- | Apply a sign to a number
applySign :: Num a => Sign -> a -> a
applySign POS = id
applySign NEG = negate

-- | Aop configuration, `p`, as in Fig. 6
data AConf = AConf
  { l1 :: Shift
  , l2 :: Shift
  , r  :: Shift
  , s  :: Sign
  }
  deriving (Eq, Show, Generic, Lift)

data AOp = AOp { p::AConf, u::Fundamental, v::Fundamental}
  deriving (Eq, Show, Generic, Lift)

-- | Aop definition, Ap, as in Eq. 1
aOp :: AConf -> Fundamental -> Fundamental -> Fundamental
aOp (AConf l1 l2 r s) u v =
  abs
  ( (            u `shiftL` l1)
     +
    (applySign s v `shiftL` l2)
  ) `shiftR` r

-- A∗(u, v) = {A p (u, v) | p is a valid configuration } − u − v.
-- Is it useful to return the fundamentals and the configuration?
-- Eq. 5

-- It is useful to extend the definition of A ∗ to sets of inputs.
-- Eq. 6
-- Eq. 7 notes that there's a sort of commutativity present for the set
-- definition.

-- What do we want?
--
-- #1
-- Interface that gets A* (the vertex fundamental set) for given inputs
-- These inputs can be a pair of fundamentals, or sets of fundamentals

-- | A type class for placing constraints on valid A-op configurations
class AOpConstr a where

  -- | The upper bound (in bits) on fundamentals generated with the given constraint
  maxBits :: a -> Fundamental

  -- | The upper bound on fundamentals generated with the given constraint
  maxF :: a -> Fundamental
  maxF a = shiftL 1 $ maxBits a

  -- | Generate the vertex fundamental set A* for given the constraint and the
  -- input fundamentals
  aStarSet :: a -> Fundamental -> Fundamental -> [(Fundamental,AOp)]


-- Let's make Aodd an implementation of this now

newtype AOdd = AOdd {_maxBits :: Int}

instance AOpConstr AOdd where

  maxBits (AOdd maxBits) = maxBits

  aStarSet constr f1 f2 =
    -- Only apply shifts to at most one edge at a time...
    let shifts = getShifts $ maxBits constr
    -- Generate AConfs for all valid l shifts, in range, and with r shifts to
    -- preseve oddness
    in [ (fOut, AOp (AConf l1 l2 r s) f1 f2)
       | (l1, l2) <- shifts
       , s        <- [POS,NEG]
       , let unshiftedVal = aOp (AConf l1 l2 0 s) f1 f2
       , let r            = countTrailingZeros unshiftedVal
       , let fOut         = shiftR unshiftedVal r
       , unshiftedVal > 1 && fOut < maxF constr
       --, fOut /= f1 && fOut /= f2
       -- TODO Removed this line to make dist 3 case 5 test work... should I do this more elegantly?
       ]

-- | Reduce a potentally even number to it's odd fundamental
divEven :: Fundamental -> Fundamental
divEven c = shiftR c (countTrailingZeros c)

-- | Reduce a list of fundamentals to a collection of unique odd fundamentals
divEvens :: [Fundamental] -> [Fundamental]
divEvens = filter (/=0) .nub . map divEven


getShifts :: Int -> [(Shift,Shift)]
getShifts = flip take shifts . (1+) . (2*)

shifts :: [(Shift,Shift)]
shifts = (0,0) : ( map (\(a,b)->(fromInteger a, fromInteger b)) .
                   concat $ transpose [ zip [1..]      (repeat 0),
                                        zip (repeat 0) [1..]
                                      ]
                 )
