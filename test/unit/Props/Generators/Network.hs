-- | Exports generators for 'NetBytes'.
module Props.Generators.Network
  ( genNet,
    genNormalizedNetBytes,
    genSomeNetSizeDown,
    genSomeNetSizeUp,
    genSomeNetDirUp,
    genSomeNetDirDown,
    genSomeNet,
  )
where

import Data.Bytes.Internal (SomeSize (..))
import Data.Bytes.Network.Direction (Direction (..), SDirection (..))
import Data.Bytes.Network.NetBytes.Internal (NetBytes (..), SomeNetSize (..))
import Data.Bytes.Network.SomeNetDir.Internal (SomeNet (..), SomeNetDir (..))
import Data.Bytes.Size (SSize (..), Size (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen
import Props.Generators.Bytes qualified as BGens
import Props.Generators.Direction qualified as DGens
import Props.Generators.Size qualified as SGens

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
      MkSomeNetSize SP <$> genNet,
      MkSomeNetSize SE <$> genNet,
      MkSomeNetSize SZ <$> genNet,
      MkSomeNetSize SY <$> genNet
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
      MkSomeNetSize SP <$> genNet,
      MkSomeNetSize SE <$> genNet,
      MkSomeNetSize SZ <$> genNet,
      MkSomeNetSize SY <$> genNet
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
    SE -> MkSomeNetSize SE $ MkNetBytes x
    SZ -> MkSomeNetSize SZ $ MkNetBytes x
    SY -> MkSomeNetSize SY $ MkNetBytes x

-- | Generates 'SomeNetDir' 'Up' over 'genNet'.
genSomeNetDirUp :: Gen (SomeNetDir s Rational)
genSomeNetDirUp = MkSomeNetDir SUp <$> genNet

-- | Generates 'SomeNetDir' 'Down' over 'genNet'.
genSomeNetDirDown :: Gen (SomeNetDir s Rational)
genSomeNetDirDown = MkSomeNetDir SDown <$> genNet

-- | Generates 'SomeNet'.
genSomeNet :: Gen (SomeNet Rational)
genSomeNet = do
  size <- SGens.genSize
  dir <- DGens.genDirection
  case size of
    B -> case dir of
      Down -> MkSomeNet SDown SB <$> genNet
      Up -> MkSomeNet SUp SB <$> genNet
    K -> case dir of
      Down -> MkSomeNet SDown SK <$> genNet
      Up -> MkSomeNet SUp SK <$> genNet
    M -> case dir of
      Down -> MkSomeNet SDown SM <$> genNet
      Up -> MkSomeNet SUp SM <$> genNet
    G -> case dir of
      Down -> MkSomeNet SDown SG <$> genNet
      Up -> MkSomeNet SUp SG <$> genNet
    T -> case dir of
      Down -> MkSomeNet SDown ST <$> genNet
      Up -> MkSomeNet SUp ST <$> genNet
    P -> case dir of
      Down -> MkSomeNet SDown SP <$> genNet
      Up -> MkSomeNet SUp SP <$> genNet
    E -> case dir of
      Down -> MkSomeNet SDown SE <$> genNet
      Up -> MkSomeNet SUp SE <$> genNet
    Z -> case dir of
      Down -> MkSomeNet SDown SZ <$> genNet
      Up -> MkSomeNet SUp SZ <$> genNet
    Y -> case dir of
      Down -> MkSomeNet SDown SY <$> genNet
      Up -> MkSomeNet SUp SY <$> genNet
