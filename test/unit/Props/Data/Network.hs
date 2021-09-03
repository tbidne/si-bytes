-- | Property tests for 'Bytes'.
module Props.Data.Network (props) where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Direction (ByteDirection (..))
import ByteTypes.Data.Network (AnyNetSize (..), NetBytes (..))
import ByteTypes.Data.Network qualified as NetBytes
import ByteTypes.Data.Size (SByteSize (..), SingByteSize (..))
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Props.Data.Network.Generators qualified as NGens
import Props.MaxRuns (MaxRuns (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Network"
    $ netBytesProps <> anyNetSizeProps

netBytesProps :: [TestTree]
netBytesProps = [convertNetProps]

anyNetSizeProps :: [TestTree]
anyNetSizeProps = []

convertNetProps :: TestTree
convertNetProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty
    "AnySize Conversions match underlying Bytes"
    $ H.withTests limit $
      H.property $ do
        anyNetSize :: (AnyNetSize 'Down Rational) <- H.forAll NGens.genSomeNetSizeDown
        convertAndTestAny anyNetSize toB toB
        convertAndTestAny anyNetSize toKB toKB
        convertAndTestAny anyNetSize toMB toMB
        convertAndTestAny anyNetSize toGB toGB
        convertAndTestAny anyNetSize toTB toTB
        convertAndTestAny anyNetSize toPB toPB

convertAndTestAny ::
  forall n d t.
  (Ord n, Show n) =>
  AnyNetSize d n ->
  (AnyNetSize d n -> NetBytes d t n) ->
  (forall s. SingByteSize s => NetBytes d s n -> NetBytes d t n) ->
  PropertyT IO ()
convertAndTestAny anySize@(MkAnyNetSize sz bytes) anyToX toX = do
  let anyConv = anyToX anySize
      bytesConv :: NetBytes d t n
      bytesConv = case sz of
        SB -> toX bytes
        SKB -> toX bytes
        SMB -> toX bytes
        SGB -> toX bytes
        STB -> toX bytes
        SPB -> toX bytes
  anyMatchesBytes anyConv bytesConv

anyMatchesBytes ::
  (Ord n, Show n) =>
  NetBytes d s n ->
  NetBytes d s n ->
  PropertyT IO ()
anyMatchesBytes anySize bytes = do
  let anyBytes = Bytes.unBytes $ NetBytes.unNetBytes anySize
      bytes' = Bytes.unBytes $ NetBytes.unNetBytes bytes
  anyBytes === bytes'
