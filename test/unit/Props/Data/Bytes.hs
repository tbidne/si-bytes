{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'Bytes'.
module Props.Data.Bytes (props) where

import ByteTypes.Class.Conversion
  ( Conversion (..),
    DecByteSize (..),
    IncByteSize (..),
  )
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Size (ByteSize (..), SByteSize (..), SingByteSize (..))
import ByteTypes.Data.Size qualified as Size
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Props.Data.Bytes.Generators qualified as Gens
import Props.Data.Size.Generators qualified as SGens
import Props.MaxRuns (MaxRuns (..))
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
    bytesGroupProps,
    bytesVectorSpaceProps
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
  TH.testProperty "Bytes B Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkBytes x) <- H.forAll (Gens.genBytes @'B)
        let bExp = x
            kExp = x .%. 1_000
            mExp = x .%. 1_000_000
            gExp = x .%. 1_000_000_000
            tExp = x .%. 1_000_000_000_000
            pExp = x .%. 1_000_000_000_000_000
        convert MkExpectedConvs {..} bytes

convertKBProps :: TestTree
convertKBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes KB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkBytes x) <- H.forAll (Gens.genBytes @'KB)
        let bExp = x .*. 1_000
            kExp = x
            mExp = x .%. 1_000
            gExp = x .%. 1_000_000
            tExp = x .%. 1_000_000_000
            pExp = x .%. 1_000_000_000_000
        convert MkExpectedConvs {..} bytes

convertMBProps :: TestTree
convertMBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes MB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkBytes x) <- H.forAll (Gens.genBytes @'MB)
        let bExp = x .*. 1_000_000
            kExp = x .*. 1_000
            mExp = x
            gExp = x .%. 1_000
            tExp = x .%. 1_000_000
            pExp = x .%. 1_000_000_000
        convert MkExpectedConvs {..} bytes

convertGBProps :: TestTree
convertGBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes GB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkBytes x) <- H.forAll (Gens.genBytes @'GB)
        let bExp = x .*. 1_000_000_000
            kExp = x .*. 1_000_000
            mExp = x .*. 1_000
            gExp = x
            tExp = x .%. 1_000
            pExp = x .%. 1_000_000
        convert MkExpectedConvs {..} bytes

convertTBProps :: TestTree
convertTBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes TB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkBytes x) <- H.forAll (Gens.genBytes @'TB)
        let bExp = x .*. 1_000_000_000_000
            kExp = x .*. 1_000_000_000
            mExp = x .*. 1_000_000
            gExp = x .*. 1_000
            tExp = x
            pExp = x .%. 1_000
        convert MkExpectedConvs {..} bytes

convertPBProps :: TestTree
convertPBProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes PB Conversions" $
    H.withTests limit $
      H.property $ do
        bytes@(MkBytes x) <- H.forAll (Gens.genBytes @'PB)
        let bExp = x .*. 1_000_000_000_000_000
            kExp = x .*. 1_000_000_000_000
            mExp = x .*. 1_000_000_000
            gExp = x .*. 1_000_000
            tExp = x .*. 1_000
            pExp = x
        convert MkExpectedConvs {..} bytes

convert :: SingByteSize s => ExpectedConvs -> Bytes s Rational -> PropertyT IO ()
convert MkExpectedConvs {..} bytes = do
  convertAndTest bExp bytes "B" toB
  convertAndTest kExp bytes "KB" toKB
  convertAndTest mExp bytes "MB" toMB
  convertAndTest gExp bytes "GB" toGB
  convertAndTest tExp bytes "TB" toTB
  convertAndTest pExp bytes "PB" toPB

convertAndTest ::
  Rational ->
  Bytes s Rational ->
  String ->
  (Bytes s Rational -> Bytes t Rational) ->
  PropertyT IO ()
convertAndTest expected bytes label convFn = do
  let expectedRed = PropUtils.reduce expected
  let resultRed = PropUtils.reduce $ Bytes.unBytes $ convFn bytes
  H.footnote $ label <> " expected: " <> show expectedRed
  H.footnote $ label <> " result: " <> show resultRed
  resultRed === expectedRed

incProps :: TestTree
incProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Increasing label reduces size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkAnySize sz bytes@(MkBytes x)) <- H.forAll Gens.genNormalizedBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SPB -> (x, Bytes.unBytes bytes)
              SB -> Size.withSingByteSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              SKB -> Size.withSingByteSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              SMB -> Size.withSingByteSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              SGB -> Size.withSingByteSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              STB -> Size.withSingByteSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected

decProps :: TestTree
decProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Decreasing label multiplies size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkAnySize sz bytes@(MkBytes x)) <- H.forAll Gens.genNormalizedBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SB -> (x, Bytes.unBytes bytes)
              SKB -> Size.withSingByteSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SMB -> Size.withSingByteSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SGB -> Size.withSingByteSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              STB -> Size.withSingByteSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SPB -> Size.withSingByteSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Normalizes bytes" $
    H.withTests limit $
      H.property $ do
        (MkAnySize sz bytes) <- H.forAll Gens.genSomeBytes
        let normalized :: AnySize Rational
            normalized = Size.withSingByteSize sz $ normalize bytes
            label = anySizeToLabel normalized
        H.footnote $ "original: " <> show bytes
        H.footnote $ "normalized: " <> show normalized
        PropUtils.isNormalized label normalized

bytesEqProps :: TestTree
bytesEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        z <- H.forAll (Gens.genBytes @'PB)
        PropUtils.eqLaws x y z

bytesOrdProps :: TestTree
bytesOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        z <- H.forAll (Gens.genBytes @'PB)
        PropUtils.ordLaws x y z

bytesGroupProps :: TestTree
bytesGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        z <- H.forAll (Gens.genBytes @'PB)
        PropUtils.groupLaws x y z

bytesVectorSpaceProps :: TestTree
bytesVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        k <- H.forAll SGens.genD
        l <- H.forAll SGens.genD
        PropUtils.vectorSpaceLaws x y k l

anySizeProps :: [TestTree]
anySizeProps =
  [ convertAnySizeProps,
    normalizeAnySizeProps,
    anySizeEqProps,
    anySizeOrdProps,
    anySizeGroupProps,
    anyVectorSpaceProps
  ]

convertAnySizeProps :: TestTree
convertAnySizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty
    "AnySize Conversions match underlying Bytes"
    $ H.withTests limit $
      H.property $ do
        anySize <- H.forAll Gens.genSomeBytes
        convertAndTestAny anySize toB toB
        convertAndTestAny anySize toKB toKB
        convertAndTestAny anySize toMB toMB
        convertAndTestAny anySize toGB toGB
        convertAndTestAny anySize toTB toTB
        convertAndTestAny anySize toPB toPB

convertAndTestAny ::
  forall n t.
  (Ord n, Show n) =>
  AnySize n ->
  (AnySize n -> Bytes t n) ->
  (forall s. SingByteSize s => Bytes s n -> Bytes t n) ->
  PropertyT IO ()
convertAndTestAny anySize@(MkAnySize sz bytes) anyToX toX =
  let anyConv = anyToX anySize
      bytesConv :: Bytes t n
      bytesConv = Size.withSingByteSize sz $ toX bytes
   in anyMatchesBytes anyConv bytesConv

anyMatchesBytes :: (Ord n, Show n) => Bytes s n -> Bytes s n -> PropertyT IO ()
anyMatchesBytes anySize bytes = do
  let anyBytes = Bytes.unBytes anySize
      bytes' = Bytes.unBytes bytes
  anyBytes === bytes'

normalizeAnySizeProps :: TestTree
normalizeAnySizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize normalization" $
    H.withTests limit $
      H.property $ do
        anySize <- H.forAll Gens.genSomeBytes
        let anyNorm = normalize anySize
            label = anySizeToLabel anyNorm
        PropUtils.isNormalized label anySize

anySizeToLabel :: AnySize n -> ByteSize
anySizeToLabel (MkAnySize sz _) = case sz of
  SB -> B
  SKB -> KB
  SMB -> MB
  SGB -> GB
  STB -> TB
  SPB -> PB

anySizeEqProps :: TestTree
anySizeEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        z <- H.forAll (Gens.genBytes @'PB)
        PropUtils.eqLaws x y z

anySizeOrdProps :: TestTree
anySizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        z <- H.forAll (Gens.genBytes @'PB)
        PropUtils.ordLaws x y z

anySizeGroupProps :: TestTree
anySizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        z <- H.forAll (Gens.genBytes @'PB)
        PropUtils.groupLaws x y z

anyVectorSpaceProps :: TestTree
anyVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'PB)
        y <- H.forAll (Gens.genBytes @'PB)
        k <- H.forAll SGens.genD
        l <- H.forAll SGens.genD
        PropUtils.vectorSpaceLaws x y k l
