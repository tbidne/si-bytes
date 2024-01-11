-- | Exports generators for 'Size'.
module Unit.Props.Generators.Size
  ( genSize,
    genD,
    genNonZero,
    genBNum,
    genKNum,
    genMNum,
    genGNum,
    genTNum,
    genPNum,
  )
where

import Data.Bytes.Size (Size (B, E, G, K, M, P, T, Y, Z))
import Data.Ratio ((%))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange

-- | Uniform distribution over 'Size'.
genSize :: Gen Size
genSize = HGen.element [B, K, M, G, T, P, E, Z, Y]

-- | Generates a linear distribution from 0 to 1,000,000,000,000,000.
genD :: Gen Rational
genD = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 500_000_000_000_000
    lower = 0
    upper = 1_000_000_000_000_000

-- | Generates a linear distribution from 0 to 1,000,000,000,000,000.
genNonZero :: Gen Rational
genNonZero = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 500_000_000_000_000
    lower = 1
    upper = 1_000_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000,000,000,000.
-- This covers the full range of (Bytes, PetaBytes).
genBNum :: Gen Rational
genBNum = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 1_000_000_000_000_000_000
    lower = 0
    upper = 2_000_000_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000,000,000.
-- When used with 'K', this covers the full range of (Bytes, PetaBytes).
genKNum :: Gen Rational
genKNum = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 1_000_000_000_000_000
    lower = 0
    upper = 2_000_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000,000.
-- When used with 'M', this covers the full range of (Bytes, PetaBytes).
genMNum :: Gen Rational
genMNum = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 1_000_000_000_000
    lower = 0
    upper = 2_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000. When used
-- with 'G', this covers the full range of (Bytes, PetaBytes).
genGNum :: Gen Rational
genGNum = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 1_000_000_000
    lower = 0
    upper = 2_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000. When used with
-- 'T', this covers the full range of (Bytes, PetaBytes).
genTNum :: Gen Rational
genTNum = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 1_000_000
    lower = 0
    upper = 2_000_000

-- | Generates a linear distribution from 0 to 2,000. When used with 'P',
-- this covers the full range of (Bytes, PetaBytes).
genPNum :: Gen Rational
genPNum = (% 1) <$> HGen.integral (HRange.linearFrom origin lower upper)
  where
    origin = 1_000
    lower = 0
    upper = 2_000
