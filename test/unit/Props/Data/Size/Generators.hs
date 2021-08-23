-- | Exports generators for 'ByteSize'.
module Props.Data.Size.Generators
  ( genByteSize,
    genD,
    genBNum,
    genKBNum,
    genMBNum,
    genGBNum,
    genTBNum,
    genPBNum,
  )
where

import ByteTypes.Data.Size (ByteSize (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange

-- | Uniform distribution over 'ByteSize'.
genByteSize :: Gen ByteSize
genByteSize = HGen.element [B, KB, MB, GB, TB, PB]

-- | Generates a linear distribution from 0 to 1,000,000,000,000,000.
genD :: Gen Double
genD = HGen.double $ HRange.linearFracFrom origin lower upper
  where
    origin = 500_000_000_000_000
    lower = 0
    upper = 1_000_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000,000,000,000.
-- This covers the full range of (Bytes, PetaBytes).
genBNum :: Gen Double
genBNum = HGen.double $ HRange.linearFracFrom origin lower upper
  where
    origin = 1_000_000_000_000_000_000
    lower = 0
    upper = 2_000_000_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000,000,000.
-- When used with 'KB', this covers the full range of (Bytes, PetaBytes).
genKBNum :: Gen Double
genKBNum = HGen.double $ HRange.linearFracFrom origin lower upper
  where
    origin = 1_000_000_000_000_000
    lower = 0
    upper = 2_000_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000,000.
-- When used with 'MB', this covers the full range of (Bytes, PetaBytes).
genMBNum :: Gen Double
genMBNum = HGen.double $ HRange.linearFracFrom origin lower upper
  where
    origin = 1_000_000_000_000
    lower = 0
    upper = 2_000_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000,000. When used
-- with 'GB', this covers the full range of (Bytes, PetaBytes).
genGBNum :: Gen Double
genGBNum = HGen.double $ HRange.linearFracFrom origin lower upper
  where
    origin = 1_000_000_000
    lower = 0
    upper = 2_000_000_000

-- | Generates a linear distribution from 0 to 2,000,000. When used with
-- 'TB', this covers the full range of (Bytes, PetaBytes).
genTBNum :: Gen Double
genTBNum = HGen.double $ HRange.linearFracFrom origin lower upper
  where
    origin = 1_000_000
    lower = 0
    upper = 2_000_000

-- | Generates a linear distribution from 0 to 2,000. When used with 'PB',
-- this covers the full range of (Bytes, PetaBytes).
genPBNum :: Gen Double
genPBNum = HGen.double $ HRange.linearFracFrom origin lower upper
  where
    origin = 1_000
    lower = 0
    upper = 2_000
