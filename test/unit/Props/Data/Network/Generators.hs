-- | Exports generators for 'NetBytes'.
module Props.Data.Network.Generators
  ( genNet,
    genSomeNetSizeDown,
    genSomeNetSizeUp,
  )
where

import ByteTypes.Data.Network
  ( AnyNetSize (..),
    ByteDirection (..),
    NetBytes (..),
  )
import ByteTypes.Data.Size (SByteSize (..))
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
