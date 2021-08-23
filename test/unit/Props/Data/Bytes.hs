{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'Bytes'.
module Props.Data.Bytes (props) where

import ByteTypes.Class.Num (BytesNum (..))
import ByteTypes.Data.Bytes
  ( AnySize (..),
    ByteSize (..),
    Bytes (..),
    Conversion (..),
    DecByteSize (..),
    IncByteSize (..),
    Normalize (..),
  )
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Utils qualified as Utils
import Hedgehog (Gen, PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange
import Props.Data.Bytes.Generators qualified as Gens
import Props.Utils qualified as PropUtils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Bytes"
    $ bytesProps <> anySizeProps

bytesProps :: [TestTree]
bytesProps =
  [ convertBProps,
    convertKBProps,
    convertMBProps,
    convertGBProps,
    convertTBProps,
    convertPBProps,
    incProps,
    decProps,
    normalizeProps,
    bytesEqProps,
    bytesOrdProps,
    bytesNumProps
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
convertBProps = TH.testProperty "Bytes B Conversions" $
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
convertKBProps = TH.testProperty "Bytes KB Conversions" $
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
convertMBProps = TH.testProperty "Bytes MB Conversions" $
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
convertGBProps = TH.testProperty "Bytes GB Conversions" $
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
convertTBProps = TH.testProperty "Bytes TB Conversions" $
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
convertPBProps = TH.testProperty "Bytes PB Conversions" $
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
        result = Bytes.unBytes $ next bytes
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
        result = Bytes.unBytes $ prev bytes
    H.footnote $ "expected: " <> show expected
    H.footnote $ " result: " <> show result
    H.assert $ Utils.epsEq result expected

normalizeProps :: TestTree
normalizeProps = TH.testProperty "Normalizes bytes" $
  H.property $ do
    (MkAnySize bytes) <- H.forAll Gens.genSomeBytes
    let normalized = normalize bytes
        label = anySizeToLabel normalized
    H.footnote $ "original: " <> show bytes
    H.footnote $ "normalized: " <> show normalized
    H.assert $ PropUtils.isNormalized label normalized

bytesEqProps :: TestTree
bytesEqProps = TH.testProperty "Bytes Eq laws" $
  H.property $ do
    (x, y, z, k) <- H.forAll gen3BytesK
    PropUtils.eqLaws x y z k

bytesOrdProps :: TestTree
bytesOrdProps = TH.testProperty "Bytes Ord laws" $
  H.property $ do
    (x, y, z, k) <- H.forAll gen3BytesK
    PropUtils.ordLaws x y z k

bytesNumProps :: TestTree
bytesNumProps = TH.testProperty "Bytes Num laws" $
  H.property $ do
    (x, y, z, k, l) <- H.forAll gen3Bytes2K
    PropUtils.numLaws x y z k l

anySizeProps :: [TestTree]
anySizeProps =
  [ convertAnySizeProps,
    normalizeAnySizeProps,
    bytesNumAnyNormProps,
    anySizeEqProps,
    anySizeOrdProps,
    anySizeNumProps
  ]

convertAnySizeProps :: TestTree
convertAnySizeProps = TH.testProperty
  "AnySize Conversions match underlying Bytes"
  $ H.property $ do
    anySize <- H.forAll Gens.genSomeBytes
    convertAndTestAny anySize toB toB
    convertAndTestAny anySize toKB toKB
    convertAndTestAny anySize toMB toMB
    convertAndTestAny anySize toGB toGB
    convertAndTestAny anySize toTB toTB
    convertAndTestAny anySize toPB toPB

convertAndTestAny ::
  (Fractional n, Ord n) =>
  AnySize n ->
  (AnySize n -> AnySize n) ->
  (forall s. Bytes s n -> Bytes t n) ->
  PropertyT IO ()
convertAndTestAny anySize@(MkAnySize bytes) anyToX toX =
  let anyConv = anyToX anySize
      bytesConv = toX bytes
   in H.assert $ anyMatchesBytes anyConv bytesConv

anyMatchesBytes :: (Fractional n, Ord n) => AnySize n -> Bytes s n -> Bool
anyMatchesBytes anySize bytes =
  let anyBytes = case anySize of
        MkAnySize b -> Bytes.unBytes b
      bytes' = Bytes.unBytes bytes
   in Utils.epsEq anyBytes bytes'

normalizeAnySizeProps :: TestTree
normalizeAnySizeProps = TH.testProperty "AnySize normalization" $
  H.property $ do
    anySize <- H.forAll Gens.genSomeBytes
    let anyNorm = normalize anySize
        label = anySizeToLabel anyNorm
    H.assert $ PropUtils.isNormalized label anySize

bytesNumAnyNormProps :: TestTree
bytesNumAnyNormProps = TH.testProperty "BytesNum for AnySize is normalized" $
  H.property $ do
    (anyOne, anyTwo, k) <- H.forAll genAnysWithMult
    let anySum = anyOne .+. anyTwo
        anyDiff = anyOne .-. anyTwo
        anyScaled = anyOne .* k
    anyNormalized anySum
    anyNormalized anyDiff
    anyNormalized anyScaled

anyNormalized :: (Fractional n, Ord n, Show n) => AnySize n -> PropertyT IO ()
anyNormalized anySize = do
  let label = anySizeToLabel anySize
  H.footnoteShow anySize
  H.assert $ PropUtils.isNormalized label anySize

anySizeToLabel :: AnySize n -> ByteSize
anySizeToLabel (MkAnySize (MkB _)) = B
anySizeToLabel (MkAnySize (MkKB _)) = KB
anySizeToLabel (MkAnySize (MkMB _)) = MB
anySizeToLabel (MkAnySize (MkGB _)) = GB
anySizeToLabel (MkAnySize (MkTB _)) = TB
anySizeToLabel (MkAnySize (MkPB _)) = PB

anySizeEqProps :: TestTree
anySizeEqProps = TH.testProperty "AnySize Eq laws" $
  H.property $ do
    (x, y, z, k) <- H.forAll gen3AnySizeK
    PropUtils.eqLaws x y z k

anySizeOrdProps :: TestTree
anySizeOrdProps = TH.testProperty "AnySize Ord laws" $
  H.property $ do
    (x, y, z, k) <- H.forAll gen3AnySizeK
    PropUtils.ordLaws x y z k

anySizeNumProps :: TestTree
anySizeNumProps = TH.testProperty "AnySize Num laws" $
  H.property $ do
    (x, y, z, k, l) <- H.forAll gen3AnySize2K
    PropUtils.numLaws x y z k l

genAnysWithMult :: Gen (AnySize Double, AnySize Double, Double)
genAnysWithMult =
  (,,)
    <$> Gens.genSomeBytes
    <*> Gens.genSomeBytes
    <*> genScalar
  where
    genScalar = HGen.double $ HRange.linearFracFrom 0 0 1_000

gen3BytesK :: Gen (Bytes 'PB Double, Bytes 'PB Double, Bytes 'PB Double, Double)
gen3BytesK =
  (,,,)
    <$> Gens.genPB
    <*> Gens.genPB
    <*> Gens.genPB
    <*> genScalar
  where
    genScalar = HGen.double $ HRange.linearFracFrom 0 0 1_000

gen3AnySizeK :: Gen (AnySize Double, AnySize Double, AnySize Double, Double)
gen3AnySizeK =
  (,,,)
    <$> Gens.genSomeBytes
    <*> Gens.genSomeBytes
    <*> Gens.genSomeBytes
    <*> genScalar
  where
    genScalar = HGen.double $ HRange.linearFracFrom 0 0 1_000

gen3Bytes2K :: Gen (Bytes 'PB Double, Bytes 'PB Double, Bytes 'PB Double, Double, Double)
gen3Bytes2K =
  (,,,,)
    <$> Gens.genPB
    <*> Gens.genPB
    <*> Gens.genPB
    <*> genScalar
    <*> genScalar
  where
    genScalar = HGen.double $ HRange.linearFracFrom 0 0 1_000

gen3AnySize2K :: Gen (AnySize Double, AnySize Double, AnySize Double, Double, Double)
gen3AnySize2K =
  (,,,,)
    <$> Gens.genSomeBytes
    <*> Gens.genSomeBytes
    <*> Gens.genSomeBytes
    <*> genScalar
    <*> genScalar
  where
    genScalar = HGen.double $ HRange.linearFracFrom 0 0 1_000
