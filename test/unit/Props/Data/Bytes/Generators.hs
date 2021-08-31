-- | Exports generators for 'Bytes'.
module Props.Data.Bytes.Generators
  ( genBytes,
    genNormalizedBytes,
    genSomeBytes,
  )
where

import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Size (ByteSize (..), SByteSize (..))
import Data.Ratio ((%))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange
import Props.Data.Size.Generators qualified as Gens

-- | Generates 'Bytes' over 'Gens.genBNum'.
genBytes :: Gen (Bytes s Rational)
genBytes = MkBytes <$> Gens.genBNum

-- | Chooses one from [B, KB, MB, ...]
genSomeBytes :: Gen (AnySize Rational)
genSomeBytes = do
  HGen.choice
    [ MkAnySize SB <$> genBytes,
      MkAnySize SKB <$> genBytes,
      MkAnySize SMB <$> genBytes,
      MkAnySize SGB <$> genBytes,
      MkAnySize STB <$> genBytes,
      MkAnySize SPB <$> genBytes
    ]

-- | Generates a normalized 'Bytes', i.e., the numeric value
-- is \[ 0 \le x < 1,000 \].
genNormalizedBytes :: Gen (AnySize Rational)
genNormalizedBytes = do
  sz <- Gens.genByteSize
  num <- gen_1_000
  pure $ case sz of
    B -> MkAnySize SB $ MkBytes num
    KB -> MkAnySize SKB $ MkBytes num
    MB -> MkAnySize SMB $ MkBytes num
    GB -> MkAnySize SGB $ MkBytes num
    TB -> MkAnySize STB $ MkBytes num
    PB -> MkAnySize SPB $ MkBytes num

gen_1_000 :: Gen Rational
gen_1_000 = (% 1) <$> HGen.integral (HRange.linearFrom 500 0 1_000)
