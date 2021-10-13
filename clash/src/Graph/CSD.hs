module Graph.CSD where

import Prelude
import Data.Bits
--import Test.QuickCheck

toBits :: Int -> [Bool]
toBits x = [testBit x i | i <- [0.. ceiling (logBase 2 $ fromIntegral x)]]

data CSDBit = Zero | Pos | Neg
  deriving (Show, Eq)

csdBitToInt :: CSDBit -> Int
csdBitToInt Zero = 0
csdBitToInt Pos = 1
csdBitToInt Neg = -1

toCSD :: [Bool] -> [CSDBit]
toCSD x = go (x ++ [last x]) False
  where
    go (xi : xj : xs) False = case (xj, xi) of
                                (False, False) -> Zero : go (xj : xs) False
                                (False, True ) -> Pos  : go (xj : xs) False
                                (True , False) -> Zero : go (xj : xs) False
                                (True , True ) -> Neg  : go (xj : xs) True
    go (xi : xj : xs) True = case (xj, xi) of
                                (False, False) -> Pos  : go (xj : xs) False
                                (False, True ) -> Zero : go (xj : xs) True
                                (True , False) -> Neg  : go (xj : xs) True
                                (True , True ) -> Zero : go (xj : xs) True
    go _              _    = []

csdCost' :: Int -> Int
csdCost' = nonZeros . toCSD . toBits
  where nonZeros = length . filter (/= Zero)

-- Property for converting to and from CSD preserves numerical value
prop_value :: Int -> Bool
prop_value x = x == sum (zipWith (*) [2^i | i<-[0..]] . map csdBitToInt . toCSD $ toBits x)

-- Property for no two adjacent non-zeros
prop_nonzeros :: Int -> Bool
prop_nonzeros x = checkAdjNonZeros . toCSD $ toBits x
  where
    checkAdjNonZeros (Pos : Pos : _) = False
    checkAdjNonZeros (Pos : Neg : _) = False
    checkAdjNonZeros (Neg : Pos : _) = False
    checkAdjNonZeros (Neg : Neg : _) = False
    checkAdjNonZeros (_   :     xs ) = checkAdjNonZeros xs
    checkAdjNonZeros []              = True

--return []
--runTests = $quickCheckAll

csdCost :: Int -> Int
csdCost c = go (shiftL c 1) (0,0)
  where go 0 (res,0) = res
        go 0 (res,1) = res + 1
        go c (res,0) = go (shiftR c 1) $ case c .&. 3 of
                         1 -> (res+1, 0)
                         3 -> (res+1, 1)
                         otherwise -> (res, 0)
        go c (res,1) = go (shiftR c 1) $ case c .&. 3 of
                         0 -> (res+1, 0)
                         2 -> (res+1, 1)
                         otherwise -> (res, 1)

csdDepth :: Int -> Int
csdDepth = ceiling . logBase 2 . fromIntegral . csdCost
