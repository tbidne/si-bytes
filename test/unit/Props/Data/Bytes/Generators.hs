-- | Exports generators for 'Bytes'.
module Props.Data.Bytes.Generators
  ( genBytes,
    genNormalizedBytes,
    genSomeBytes,
  )
where

import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Size (SSize (..), Size (..))
import Data.Ratio ((%))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange
import Props.Data.Size.Generators qualified as Gens

-- | Generates 'Bytes' over 'Gens.genBNum'.
genBytes :: Gen (Bytes s Rational)
genBytes = MkBytes <$> Gens.genBNum

-- | Chooses one from [B, K, M, ...]
genSomeBytes :: Gen (AnySize Rational)
genSomeBytes = do
  HGen.choice
    [ MkAnySize SB <$> genBytes,
      MkAnySize SK <$> genBytes,
      MkAnySize SM <$> genBytes,
      MkAnySize SG <$> genBytes,
      MkAnySize ST <$> genBytes,
      MkAnySize SP <$> genBytes
    ]

-- | Generates a normalized 'Bytes', i.e., the numeric value
-- is \[ 0 \le x < 1,000 \].
genNormalizedBytes :: Gen (AnySize Rational)
genNormalizedBytes = do
  sz <- Gens.genSize
  num <- gen_1_000
  pure $ case sz of
    B -> MkAnySize SB $ MkBytes num
    K -> MkAnySize SK $ MkBytes num
    M -> MkAnySize SM $ MkBytes num
    G -> MkAnySize SG $ MkBytes num
    T -> MkAnySize ST $ MkBytes num
    P -> MkAnySize SP $ MkBytes num

gen_1_000 :: Gen Rational
gen_1_000 = (% 1) <$> HGen.integral (HRange.linearFrom 500 0 1_000)
