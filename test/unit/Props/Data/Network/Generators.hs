-- | Exports generators for 'NetBytes'.
module Props.Data.Network.Generators
  ( genNet,
    genNormalizedNetBytes,
    genSomeNetSizeDown,
    genSomeNetSizeUp,
  )
where

import ByteTypes.Data.Bytes (AnySize (..))
import ByteTypes.Data.Direction (ByteDirection (..))
import ByteTypes.Data.Network.NetBytes (AnyNetSize (..), NetBytes (..))
import ByteTypes.Data.Size (ByteSize (..), SByteSize (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange
import Props.Data.Bytes.Generators qualified as BGens
import Props.Data.Size.Generators qualified as SGens

-- | Generates 'NetBytes' 'Down' over 'BGens.genBytes'.
genNet :: Gen (NetBytes d s Rational)
genNet = MkNetBytes <$> BGens.genBytes

-- | Generates 'AnyNetSize' 'Down' over 'BGens.genNet'.
genSomeNetSizeDown :: Gen (AnyNetSize 'Down Rational)
genSomeNetSizeDown =
  HGen.choice
    [ MkAnyNetSize SB <$> genNet,
      MkAnyNetSize SKB <$> genNet,
      MkAnyNetSize SMB <$> genNet,
      MkAnyNetSize SGB <$> genNet,
      MkAnyNetSize STB <$> genNet,
      MkAnyNetSize SPB <$> genNet
    ]

-- | Generates 'AnyNetSize' 'Up' over 'BGens.genNet'.
genSomeNetSizeUp :: Gen (AnyNetSize 'Up Rational)
genSomeNetSizeUp =
  HGen.choice
    [ MkAnyNetSize SB <$> genNet,
      MkAnyNetSize SKB <$> genNet,
      MkAnyNetSize SMB <$> genNet,
      MkAnyNetSize SGB <$> genNet,
      MkAnyNetSize STB <$> genNet,
      MkAnyNetSize SPB <$> genNet
    ]

-- | Generates a normalized 'Bytes', i.e., the numeric value
-- is \[ 0 \le x < 1,000 \].
genNormalizedNetBytes :: Gen (AnyNetSize 'Up Rational)
genNormalizedNetBytes = do
  (MkAnySize sz x) <- BGens.genNormalizedBytes
  pure $ case sz of
    SB -> MkAnyNetSize SB $ MkNetBytes x
    SKB -> MkAnyNetSize SKB $ MkNetBytes x
    SMB -> MkAnyNetSize SMB $ MkNetBytes x
    SGB -> MkAnyNetSize SGB $ MkNetBytes x
    STB -> MkAnyNetSize STB $ MkNetBytes x
    SPB -> MkAnyNetSize SPB $ MkNetBytes x
