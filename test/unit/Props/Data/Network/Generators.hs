-- | Exports generators for 'NetBytes'.
module Props.Data.Network.Generators
  ( genNet,
    genNormalizedNetBytes,
    genSomeNetSizeDown,
    genSomeNetSizeUp,
  )
where

import ByteTypes.Data.Bytes (AnySize (..))
import ByteTypes.Data.Direction (Direction (..))
import ByteTypes.Data.Network.NetBytes (AnyNetSize (..), NetBytes (..))
import ByteTypes.Data.Size (SSize (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Props.Data.Bytes.Generators qualified as BGens

-- | Generates 'NetBytes' 'Down' over 'BGens.genBytes'.
genNet :: Gen (NetBytes d s Rational)
genNet = MkNetBytes <$> BGens.genBytes

-- | Generates 'AnyNetSize' 'Down' over 'BGens.genNet'.
genSomeNetSizeDown :: Gen (AnyNetSize 'Down Rational)
genSomeNetSizeDown =
  HGen.choice
    [ MkAnyNetSize SB <$> genNet,
      MkAnyNetSize SK <$> genNet,
      MkAnyNetSize SM <$> genNet,
      MkAnyNetSize SG <$> genNet,
      MkAnyNetSize ST <$> genNet,
      MkAnyNetSize SP <$> genNet
    ]

-- | Generates 'AnyNetSize' 'Up' over 'BGens.genNet'.
genSomeNetSizeUp :: Gen (AnyNetSize 'Up Rational)
genSomeNetSizeUp =
  HGen.choice
    [ MkAnyNetSize SB <$> genNet,
      MkAnyNetSize SK <$> genNet,
      MkAnyNetSize SM <$> genNet,
      MkAnyNetSize SG <$> genNet,
      MkAnyNetSize ST <$> genNet,
      MkAnyNetSize SP <$> genNet
    ]

-- | Generates a normalized 'Bytes', i.e., the numeric value
-- is \[ 0 \le x < 1,000 \].
genNormalizedNetBytes :: Gen (AnyNetSize 'Up Rational)
genNormalizedNetBytes = do
  (MkAnySize sz x) <- BGens.genNormalizedBytes
  pure $ case sz of
    SB -> MkAnyNetSize SB $ MkNetBytes x
    SK -> MkAnyNetSize SK $ MkNetBytes x
    SM -> MkAnyNetSize SM $ MkNetBytes x
    SG -> MkAnyNetSize SG $ MkNetBytes x
    ST -> MkAnyNetSize ST $ MkNetBytes x
    SP -> MkAnyNetSize SP $ MkNetBytes x
