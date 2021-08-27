module ExampleFilters where

import Clash.Prelude

import Filter.FFA
import Filter.Serial
import Filter.Polyphase
import Graph.MCM
import ExampleCoeffs

--------------------------------------------------------------------------------
-- Serial filters
--------------------------------------------------------------------------------

-- Let's define a simple direct form FIR filter with bitgrowth at the type-level
firSerialDirect :: (KnownNat c, KnownNat a, KnownNat n, HiddenClockResetEnable dom)
                => Vec (1+n) (Signed c)
                -> Signal dom (Signed a)
                -> Signal dom (Signed (a+c+CLog 2 (1+n)))
firSerialDirect coeffs x = firDirect (map resize coeffs) (fmap resize x)

-- Note that we can use the "@" syntax to supply the type variables `c`, `a`,
-- `n`, and `dom`. Let's supply values for the wordlengths `c` and `a` to
-- specialise the direct form FIR. Use the REPL to check that the coefficients
-- are now 16 bit and the input is 18 bit.
firSerialDirect_C16A18 = firSerialDirect @16 @18

-- Let's define a serial filter that uses a MCM block generated by Hcub now.
-- Note we need to supply the coefficients directly here since the structure of
-- the MCM block (generated _at compile-time_) will depend on the coefficient
-- values. We'll use the set of band-pass coefficients defined in ExampleCoeffs.
firSerialMCM_BP :: (KnownNat a, HiddenClockResetEnable dom)
                => Signal dom (Signed a)
                -> Signal dom (Signed (a+16+CLog 2 32))
                -- 16 = coefficient wordlength, 32 = number of coefficients
firSerialMCM_BP x = firTransposeMCM
                      $(mcmPipelinedHwTH HcubShallow (toList $ map fromIntegral coeffsBpls))
                      (fmap resize x)

-- Again, we can specialise the input wordlength using the "@" syntax or via a
-- top-level type definition
firSerialMCM_BP_A12 = firSerialMCM_BP @12

--------------------------------------------------------------------------------
-- Parallel polyphase filters
--------------------------------------------------------------------------------

-- Let's generate a polyphase filter with direct form sub-filters. Again, we can
-- just leave the coefficients as a parameter here. Note a p-parallel polyphase
-- filter includes (p-1) extra additions to recombine the outputs --- this will
-- affect our bit growth, as reflected in the type below.
firPolyDirect :: forall p c a n dom
              .  (KnownNat p, KnownNat c, KnownNat a, KnownNat n, HiddenClockResetEnable dom, 1<=p)
              => Vec ((1+n)*p) (Signed c)
              -> Signal dom (Vec p (Signed a))
              -> Signal dom (Vec p (Signed (a+c + CLog 2 (1+n) + CLog 2 p)))
firPolyDirect coeffs xs = polyphase (SNat :: SNat p) firDirect (map resize coeffs) (fmap (map resize) xs)

-- Now we generate a polyphase FIR filter using an MCM block. We need to supply
-- the coefficients and parallelisation factor directly, so we'll use the
-- low-pass example set with x8 samples per cycle. This time, we'll use the
-- RAGn MCM algorithm instead of Hcub.
firPolyMCM_LP :: (KnownNat a, HiddenClockResetEnable dom)
              => Signal dom (Vec 8 (Signed a))
              -> Signal dom (Vec 8 (Signed (a+16+CLog 2 8 + CLog 2 8))
              -- 8 = parallelism, 16 = coeff wordlength, (8*8) = number of coefficients
firPolyMCM_LP xs = polyphase_MCM
                     (SNat :: SNat 8)
                     $(mcmPipelinedHwTH RAGn (toList $ map fromIntegral coeffsLpls))
                     (fmap (map resize) xs)

--------------------------------------------------------------------------------
-- Parallel FFA filters
--------------------------------------------------------------------------------

-- Finally we'll make some Fast FIR Algorithm (FFA) filters. Just like the MCM
-- generators, the FFA code (ab)uses Template Haskell, so we'll need at least
-- the parallelisation factor ready at compile time. We'll demonstrate with x16
-- samples per cycle. Let's start with direct form sub filters first. Notice
-- that we incur extra additions (and longer wordlengths!) while reducing the
-- number of required multiplies.
firFFADirect :: forall c a n dom
              .  (KnownNat c, KnownNat a, KnownNat n, HiddenClockResetEnable dom)
              => Vec ((1+n)*16) (Signed c)
              -> Signal dom (Vec 16 (Signed a))
              -> Signal dom (Vec 16 (Signed (a+c+CLog 2 (1+n)+ 4*(CLog 2 16))))
firFFADirect coeffs xs = $(genFFA (SNat :: SNat 16))
                             firDirect
                             (map resize coeffs)
                             (fmap (map resize) xs)

-- Now an FFA structure with MCM-based sub-filters. We need to ensure that all
-- subfilters have the same latency now (this was not an issue with `polyphase`)
-- so we'll use the `mcmPipelinedDepthHwTH` function this time. That enforces a
-- _minimum_ depth, so choose the HcubShallow or RSG algorithms with minimum
-- depth >=3 for best results... these algorithms aim to produce predictable,
-- low depth graphs. (Perhaps this tool should infer the minimum depth for us!)
-- We'll go for x8 parallelism with the high-pass example coefficients
firFFA_MCM_HP :: forall a dom
              .  (KnownNat a, HiddenClockResetEnable dom)
              => Signal dom (Vec 8 (Signed a))
              -> Signal dom (Vec 8 (Signed (a+16+CLog 2 16+ 4*(CLog 2 8))))
              -- 8 = parallelism, 16 = coefficient wordlength, 16 = number of coefficients
firFFA_MCM_HP xs = $(genFFA_MCM (mcmPipelinedDepthHwTH HcubShallow 3)
                                (SNat :: SNat 8)
                                (map fromIntegral coeffsHpls)
                    ) (fmap (map resize) xs)

--------------------------------------------------------------------------------
-- Synthesis Example
--------------------------------------------------------------------------------

-- We can take any of the previous filters and get Clash to generate
-- VHDL/verilog by supplying a few final annotations. We need to resolve all
-- type variable with "@"s or by supplying an explicit type definition. As an
-- example, we'll expose `firFFA_MCM_HP` as our top-level circuit below. We're
-- setting the input wordlength to 16 bits and the synthesis domain our own
-- "SystemNR" (standard domain with active low resets).
createDomain vSystem{vName="SystemNR", vResetPolarity=ActiveLow}

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "fir_dut"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "x" ]
    , t_output = PortName "y"
    }) #-}

topEntity c r x = exposeClockResetEnable
                    (firFFA_MCM_HP @16 @SystemNR x)
                    c r (toEnable $ pure True)
