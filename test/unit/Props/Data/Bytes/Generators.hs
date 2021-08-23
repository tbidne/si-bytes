-- | Exports generators for 'Bytes'.
module Props.Data.Bytes.Generators
  ( genB,
    genKB,
    genMB,
    genGB,
    genTB,
    genPB,
    genNormalizedBytes,
    genSomeBytes,
  )
where

import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Size (ByteSize (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange
import Props.Data.Size.Generators qualified as Gens

-- | Generates 'Bytes' over 'Gens.genBNum'.
genB :: Gen (Bytes 'B Double)
genB = MkB <$> Gens.genBNum

-- | Generates 'Bytes' over 'Gens.genKBNum'.
genKB :: Gen (Bytes 'KB Double)
genKB = MkKB <$> Gens.genKBNum

-- | Generates 'Bytes' over 'Gens.genMBNum'.
genMB :: Gen (Bytes 'MB Double)
genMB = MkMB <$> Gens.genMBNum

-- | Generates 'Bytes' over 'Gens.genGBNum'.
genGB :: Gen (Bytes 'GB Double)
genGB = MkGB <$> Gens.genGBNum

-- | Generates 'Bytes' over 'Gens.genTBNum'.
genTB :: Gen (Bytes 'TB Double)
genTB = MkTB <$> Gens.genTBNum

-- | Generates 'Bytes' over 'Gens.genPBNum'.
genPB :: Gen (Bytes 'PB Double)
genPB = MkPB <$> Gens.genPBNum

-- | Chooses one from [genB, genKB, ... genPB]
genSomeBytes :: Gen (AnySize Double)
genSomeBytes =
  HGen.choice
    [ MkAnySize <$> genB,
      MkAnySize <$> genKB,
      MkAnySize <$> genMB,
      MkAnySize <$> genGB,
      MkAnySize <$> genTB,
      MkAnySize <$> genPB
    ]

-- | Generates a normalized 'Bytes', i.e., the numeric value
-- is \[ 0 \le x < 1,000 \].
genNormalizedBytes :: Gen (AnySize Double)
genNormalizedBytes = do
  sz <- Gens.genByteSize
  num <- gen_1_000
  pure $ case sz of
    B -> MkAnySize $ MkB num
    KB -> MkAnySize $ MkKB num
    MB -> MkAnySize $ MkMB num
    GB -> MkAnySize $ MkGB num
    TB -> MkAnySize $ MkTB num
    PB -> MkAnySize $ MkPB num

gen_1_000 :: Gen Double
gen_1_000 = HGen.double $ HRange.linearFracFrom 500 0 1_000
