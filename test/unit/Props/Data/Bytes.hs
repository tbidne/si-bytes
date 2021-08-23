{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'Bytes'.
module Props.Data.Bytes (props) where

import ByteTypes.Data.Bytes (AnySize (..), Bytes (..), Conversion (..))
import ByteTypes.Data.Bytes qualified as Bytes
import Hedgehog (PropertyT)
import Hedgehog qualified as H
import Props.Data.Bytes.Generators qualified as Gens
import Props.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Bytes"
    [ convertBProps,
      convertKBProps,
      convertMBProps,
      convertGBProps,
      convertTBProps,
      convertPBProps,
      incProps,
      decProps,
      normalizeProps
    ]

data ExpectedConvs = MkExpectedConvs
  { bExp :: Double,
    kExp :: Double,
    mExp :: Double,
    gExp :: Double,
    tExp :: Double,
    pExp :: Double
  }

convertBProps :: TestTree
convertBProps = TH.testProperty "B Conversions" $
  H.property $ do
    bytes@(MkB x) <- H.forAll Gens.genB
    let bExp = x
        kExp = x / 1_000
        mExp = x / 1_000_000
        gExp = x / 1_000_000_000
        tExp = x / 1_000_000_000_000
        pExp = x / 1_000_000_000_000_000
    convert MkExpectedConvs {..} bytes

convertKBProps :: TestTree
convertKBProps = TH.testProperty "KB Conversions" $
  H.property $ do
    bytes@(MkKB x) <- H.forAll Gens.genKB
    let bExp = x * 1_000
        kExp = x
        mExp = x / 1_000
        gExp = x / 1_000_000
        tExp = x / 1_000_000_000
        pExp = x / 1_000_000_000_000
    convert MkExpectedConvs {..} bytes

convertMBProps :: TestTree
convertMBProps = TH.testProperty "MB Conversions" $
  H.property $ do
    bytes@(MkMB x) <- H.forAll Gens.genMB
    let bExp = x * 1_000_000
        kExp = x * 1_000
        mExp = x
        gExp = x / 1_000
        tExp = x / 1_000_000
        pExp = x / 1_000_000_000
    convert MkExpectedConvs {..} bytes

convertGBProps :: TestTree
convertGBProps = TH.testProperty "GB Conversions" $
  H.property $ do
    bytes@(MkGB x) <- H.forAll Gens.genGB
    let bExp = x * 1_000_000_000
        kExp = x * 1_000_000
        mExp = x * 1_000
        gExp = x
        tExp = x / 1_000
        pExp = x / 1_000_000
    convert MkExpectedConvs {..} bytes

convertTBProps :: TestTree
convertTBProps = TH.testProperty "TB Conversions" $
  H.property $ do
    bytes@(MkTB x) <- H.forAll Gens.genTB
    let bExp = x * 1_000_000_000_000
        kExp = x * 1_000_000_000
        mExp = x * 1_000_000
        gExp = x * 1_000
        tExp = x
        pExp = x / 1_000
    convert MkExpectedConvs {..} bytes

convertPBProps :: TestTree
convertPBProps = TH.testProperty "PB Conversions" $
  H.property $ do
    bytes@(MkPB x) <- H.forAll Gens.genPB
    let bExp = x * 1_000_000_000_000_000
        kExp = x * 1_000_000_000_000
        mExp = x * 1_000_000_000
        gExp = x * 1_000_000
        tExp = x * 1_000
        pExp = x
    convert MkExpectedConvs {..} bytes

convert :: ExpectedConvs -> Bytes s Double -> PropertyT IO ()
convert MkExpectedConvs {..} bytes = do
  convertAndTest bExp bytes "B" toB
  convertAndTest kExp bytes "KB" toKB
  convertAndTest mExp bytes "MB" toMB
  convertAndTest gExp bytes "GB" toGB
  convertAndTest tExp bytes "TB" toTB
  convertAndTest pExp bytes "PB" toPB

convertAndTest ::
  (Fractional n, Ord n, Show n) =>
  n ->
  Bytes s n ->
  String ->
  (Bytes s n -> Bytes t n) ->
  PropertyT IO ()
convertAndTest expected bytes label convFn = do
  let result = Bytes.unBytes $ convFn bytes
  H.footnote $ label <> " expected: " <> show expected
  H.footnote $ label <> " result: " <> show result
  H.assert $ Utils.epsEq result expected

incProps :: TestTree
incProps = TH.testProperty "Increasing label reduces size by 1,000" $
  H.property $ do
    (MkAnySize bytes) <- H.forAll Gens.genNormalizedBytes
    let expected = case bytes of
          MkPB x -> x
          x -> Bytes.unBytes x / 1_000
        result = Bytes.unBytes $ Bytes.next bytes
    H.footnote $ "expected: " <> show expected
    H.footnote $ " result: " <> show result
    H.assert $ Utils.epsEq result expected

decProps :: TestTree
decProps = TH.testProperty "Decreasing label multiplies size by 1,000" $
  H.property $ do
    (MkAnySize bytes) <- H.forAll Gens.genNormalizedBytes
    let expected = case bytes of
          MkB x -> x
          x -> Bytes.unBytes x * 1_000
        result = Bytes.unBytes $ Bytes.prev bytes
    H.footnote $ "expected: " <> show expected
    H.footnote $ " result: " <> show result
    H.assert $ Utils.epsEq result expected

normalizeProps :: TestTree
normalizeProps = TH.testProperty "Normalizes bytes" $
  H.property $ do
    (MkAnySize bytes) <- H.forAll Gens.genSomeBytes
    let normalized = Bytes.normalize bytes
        expectation = case normalized of
          MkAnySize (MkB x) -> x < 1_000
          MkAnySize (MkPB x) -> x >= 1
          MkAnySize bytes' ->
            let x = Bytes.unBytes bytes'
             in x >= 1 && x < 1_000
    H.footnote $ "original: " <> show bytes
    H.footnote $ "normalized: " <> show normalized
    H.assert expectation
