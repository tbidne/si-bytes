-- | Exports generators for 'Bytes'.
module ByteTypes.Props.Data.Bytes.Generators
  ( genBytes,
    genNormalizedBytes,
    genSomeBytes,
  )
where

import ByteTypes.Data.Bytes (Bytes (..), SomeSize (..))
import ByteTypes.Data.Size (SSize (..), Size (..))
import ByteTypes.Props.Data.Size.Generators qualified as Gens
import Data.Ratio ((%))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange

-- | Generates 'Bytes' over 'Gens.genBNum'.
genBytes :: Gen (Bytes s Rational)
genBytes = MkBytes <$> Gens.genBNum

-- | Chooses one from [B, K, M, ...]
genSomeBytes :: Gen (SomeSize Rational)
genSomeBytes = do
  HGen.choice
    [ MkSomeSize SB <$> genBytes,
      MkSomeSize SK <$> genBytes,
      MkSomeSize SM <$> genBytes,
      MkSomeSize SG <$> genBytes,
      MkSomeSize ST <$> genBytes,
      MkSomeSize SP <$> genBytes
    ]

-- | Generates a normalized 'Bytes', i.e., the numeric value
-- is \[ 0 \le x < 1,000 \].
genNormalizedBytes :: Gen (SomeSize Rational)
genNormalizedBytes = do
  sz <- Gens.genSize
  num <- gen_1_000
  pure $ case sz of
    B -> MkSomeSize SB $ MkBytes num
    K -> MkSomeSize SK $ MkBytes num
    M -> MkSomeSize SM $ MkBytes num
    G -> MkSomeSize SG $ MkBytes num
    T -> MkSomeSize ST $ MkBytes num
    P -> MkSomeSize SP $ MkBytes num

gen_1_000 :: Gen Rational
gen_1_000 = (% 1) <$> HGen.integral (HRange.linearFrom 500 0 1_000)
