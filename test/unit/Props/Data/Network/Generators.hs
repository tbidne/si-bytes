-- | Exports generators for 'NetBytes'.
module Props.Data.Network.Generators
  ( genNet,
    genNormalizedNetBytes,
    genSomeNetSizeDown,
    genSomeNetSizeUp,
  )
where

import ByteTypes.Data.Bytes (SomeSize (..))
import ByteTypes.Data.Direction (Direction (..))
import ByteTypes.Data.Network.NetBytes (NetBytes (..), SomeNetSize (..))
import ByteTypes.Data.Size (SSize (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Props.Data.Bytes.Generators qualified as BGens

-- | Generates 'NetBytes' 'Down' over 'BGens.genBytes'.
genNet :: Gen (NetBytes d s Rational)
genNet = MkNetBytes <$> BGens.genBytes

-- | Generates 'SomeNetSize' 'Down' over 'BGens.genNet'.
genSomeNetSizeDown :: Gen (SomeNetSize 'Down Rational)
genSomeNetSizeDown =
  HGen.choice
    [ MkSomeNetSize SB <$> genNet,
      MkSomeNetSize SK <$> genNet,
      MkSomeNetSize SM <$> genNet,
      MkSomeNetSize SG <$> genNet,
      MkSomeNetSize ST <$> genNet,
      MkSomeNetSize SP <$> genNet
    ]

-- | Generates 'SomeNetSize' 'Up' over 'BGens.genNet'.
genSomeNetSizeUp :: Gen (SomeNetSize 'Up Rational)
genSomeNetSizeUp =
  HGen.choice
    [ MkSomeNetSize SB <$> genNet,
      MkSomeNetSize SK <$> genNet,
      MkSomeNetSize SM <$> genNet,
      MkSomeNetSize SG <$> genNet,
      MkSomeNetSize ST <$> genNet,
      MkSomeNetSize SP <$> genNet
    ]

-- | Generates a normalized 'Bytes', i.e., the numeric value
-- is \[ 0 \le x < 1,000 \].
genNormalizedNetBytes :: Gen (SomeNetSize 'Up Rational)
genNormalizedNetBytes = do
  (MkSomeSize sz x) <- BGens.genNormalizedBytes
  pure $ case sz of
    SB -> MkSomeNetSize SB $ MkNetBytes x
    SK -> MkSomeNetSize SK $ MkNetBytes x
    SM -> MkSomeNetSize SM $ MkNetBytes x
    SG -> MkSomeNetSize SG $ MkNetBytes x
    ST -> MkSomeNetSize ST $ MkNetBytes x
    SP -> MkSomeNetSize SP $ MkNetBytes x
