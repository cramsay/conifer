module ExampleCoeffs (
   coeffsHpls
  ,coeffsLpls
  ,coeffsBpls
  ,coeffsMovAvg
  ) where

import Clash.Prelude

-- For filter weight design, see https://uk.mathworks.com/help/signal/ug/fir-filter-design.html
-- We only want an even number of weights, so types II and IV.
--
-- Type IV (even coeffs with antisymetry)
-- High pass with transition between 0.6 and 0.7
-- Designed with least squares algo
--
-- > h=firls(15,[0 .6 .7 1],[0 0 1 1],'h');
-- > h=round(h * (2^(16-1)-1))
coeffsHpls :: Vec 16 (Signed 16)
coeffsHpls = $(listToVecTH
               [(856 :: Signed 16),(-889),(-378),1966,(-1765),(-1551),6830
               ,(-10862),10862,(-6830),1551,1765,(-1966),378,889,(-856)])

-- Type IV band pass between .45 and .55 with .05 transitions and 32 weights
--
-- > h=firls(31,[0 .4 .45 .55 .6 1],[0 0 1 1 0 0],'h');
-- > h=round(h * (2^(16-1)-1))
coeffsBpls :: Vec 32 (Signed 16)
coeffsBpls = $(listToVecTH
              [(-221 :: Signed 16), (-93), (-50), (-255), 497, 768, (-1092)
              ,(-1402), 1767 ,2077, (-2428), (-2693), 2970, 3149, (-3301)
              ,(-3364), 3364, 3301, (-3149), (-2970), 2693, 2428, (-2077)
              ,(-1767), 1402, 1092,(-768), (-497), 255, 50, 93, 221
              ])

-- Type II low pass between 0.3 and 0.4 with 64 weights
--
-- > h=firls(63,[0 0.3 0.4 1],[1 1 0 0]);
-- > h=round(h * (2^(16-1)-1))
coeffsLpls :: Vec 64 (Signed 16)
coeffsLpls = $(listToVecTH
               [(1 :: Signed 16), 14, 17, (-5), (-37), (-36), 19, 78, 60, (-50)
               ,(-142), (-83), 111, 234, 97, (-214), (-358), (-90), 383, 523
               ,40, (-656), (-747), 95, 1130, 1095, (-427), (-2152), (-1869)
               ,1588, 6897, 10880, 10880, 6897, 1588, (-1869), (-2152), (-427)
               ,1095, 1130, 95, (-747), (-656), 40, 523, 383, (-90), (-358)
               ,(-214), 97, 234, 111, (-83), (-142), (-50), 60, 78, 19, (-36)
               ,(-37), (-5), 17, 14, 1
               ])

-- Set of 0xFFFF coefficients, for a scaled moving average.
-- This is really just to test for overflow!
coeffsMovAvg :: Vec 16 (Signed 16)
coeffsMovAvg = repeat maxBound
