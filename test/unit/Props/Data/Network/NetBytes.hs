{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'NetBytes'.
module Props.Data.Network.NetBytes (props) where

import ByteTypes.Class.Conversion (Conversion (..), DecByteSize (..), IncByteSize (..))
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Data.Bytes (Bytes (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Direction (ByteDirection (..))
import ByteTypes.Data.Network qualified as NetBytes
import ByteTypes.Data.Network.NetBytes (AnyNetSize (..), NetBytes (..))
import ByteTypes.Data.Size (ByteSize (..), SByteSize (..), SingByteSize (..))
import ByteTypes.Data.Size qualified as Size
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Props.Data.Network.Generators qualified as NGens
import Props.MaxRuns (MaxRuns (..))
import Props.Utils qualified as PropUtils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Network.NetBytes"
    $ netBytesProps <> anyNetSizeProps

netBytesProps :: [TestTree]
netBytesProps =
  [ convertBProps,
    convertKBProps,
    convertMBProps,
    convertGBProps,
    convertTBProps,
    convertPBProps,
    incProps,
    decProps
  ]

data ExpectedConvs = MkExpectedConvs
  { bExp :: Rational,
    kExp :: Rational,
    mExp :: Rational,
    gExp :: Rational,
    tExp :: Rational,
    pExp :: Rational
  }

convertBProps :: TestTree
convertBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes B Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkNetBytes (MkBytes x)) <- H.forAll (NGens.genNet @'Up @'B)
        let bExp = x
            kExp = x .%. 1_000
            mExp = x .%. 1_000_000
            gExp = x .%. 1_000_000_000
            tExp = x .%. 1_000_000_000_000
            pExp = x .%. 1_000_000_000_000_000
        convert MkExpectedConvs {..} bytes

convertKBProps :: TestTree
convertKBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes KB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkNetBytes (MkBytes x)) <- H.forAll (NGens.genNet @'Up @'KB)
        let bExp = x .*. 1_000
            kExp = x
            mExp = x .%. 1_000
            gExp = x .%. 1_000_000
            tExp = x .%. 1_000_000_000
            pExp = x .%. 1_000_000_000_000
        convert MkExpectedConvs {..} bytes

convertMBProps :: TestTree
convertMBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes MB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkNetBytes (MkBytes x)) <- H.forAll (NGens.genNet @'Up @'MB)
        let bExp = x .*. 1_000_000
            kExp = x .*. 1_000
            mExp = x
            gExp = x .%. 1_000
            tExp = x .%. 1_000_000
            pExp = x .%. 1_000_000_000
        convert MkExpectedConvs {..} bytes

convertGBProps :: TestTree
convertGBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes GB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkNetBytes (MkBytes x)) <- H.forAll (NGens.genNet @'Up @'GB)
        let bExp = x .*. 1_000_000_000
            kExp = x .*. 1_000_000
            mExp = x .*. 1_000
            gExp = x
            tExp = x .%. 1_000
            pExp = x .%. 1_000_000
        convert MkExpectedConvs {..} bytes

convertTBProps :: TestTree
convertTBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes TB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkNetBytes (MkBytes x)) <- H.forAll (NGens.genNet @'Up @'TB)
        let bExp = x .*. 1_000_000_000_000
            kExp = x .*. 1_000_000_000
            mExp = x .*. 1_000_000
            gExp = x .*. 1_000
            tExp = x
            pExp = x .%. 1_000
        convert MkExpectedConvs {..} bytes

convertPBProps :: TestTree
convertPBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes PB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkNetBytes (MkBytes x)) <- H.forAll (NGens.genNet @'Up @'PB)
        let bExp = x .*. 1_000_000_000_000_000
            kExp = x .*. 1_000_000_000_000
            mExp = x .*. 1_000_000_000
            gExp = x .*. 1_000_000
            tExp = x .*. 1_000
            pExp = x
        convert MkExpectedConvs {..} bytes

convert :: SingByteSize s => ExpectedConvs -> NetBytes d s Rational -> PropertyT IO ()
convert MkExpectedConvs {..} bytes = do
  convertAndTest bExp bytes "B" toB
  convertAndTest kExp bytes "KB" toKB
  convertAndTest mExp bytes "MB" toMB
  convertAndTest gExp bytes "GB" toGB
  convertAndTest tExp bytes "TB" toTB
  convertAndTest pExp bytes "PB" toPB

incProps :: TestTree
incProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Increasing label reduces size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkAnyNetSize sz bytes@(MkNetBytes (MkBytes x))) <- H.forAll NGens.genNormalizedNetBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SPB -> (x, netToNum bytes)
              SB -> Size.withSingByteSize sz (x .%. 1_000, netToNum (next bytes))
              SKB -> Size.withSingByteSize sz (x .%. 1_000, netToNum (next bytes))
              SMB -> Size.withSingByteSize sz (x .%. 1_000, netToNum (next bytes))
              SGB -> Size.withSingByteSize sz (x .%. 1_000, netToNum (next bytes))
              STB -> Size.withSingByteSize sz (x .%. 1_000, netToNum (next bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected

decProps :: TestTree
decProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Decreasing label multiplies size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkAnyNetSize sz bytes@(MkNetBytes (MkBytes x))) <- H.forAll NGens.genNormalizedNetBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SB -> (x, netToNum bytes)
              SKB -> Size.withSingByteSize sz (x .*. 1_000, netToNum (prev bytes))
              SMB -> Size.withSingByteSize sz (x .*. 1_000, netToNum (prev bytes))
              SGB -> Size.withSingByteSize sz (x .*. 1_000, netToNum (prev bytes))
              STB -> Size.withSingByteSize sz (x .*. 1_000, netToNum (prev bytes))
              SPB -> Size.withSingByteSize sz (x .*. 1_000, netToNum (prev bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected

anyNetSizeProps :: [TestTree]
anyNetSizeProps = [convertNetProps]

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

convertAndTest ::
  Rational ->
  NetBytes d s Rational ->
  String ->
  (NetBytes d s Rational -> NetBytes d t Rational) ->
  PropertyT IO ()
convertAndTest expected bytes label convFn = do
  let expectedRed = PropUtils.reduce expected
  let resultRed = PropUtils.reduce $ Bytes.unBytes $ NetBytes.unNetBytes $ convFn bytes
  H.footnote $ label <> " expected: " <> show expectedRed
  H.footnote $ label <> " result: " <> show resultRed
  resultRed === expectedRed

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

netToNum :: NetBytes d s n -> n
netToNum = Bytes.unBytes . NetBytes.unNetBytes
