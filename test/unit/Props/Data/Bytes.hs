{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'Bytes'.
module Props.Data.Bytes (props) where

import ByteTypes.Class.Conversion
  ( Conversion (..),
    DecSize (..),
    IncSize (..),
  )
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Size (SSize (..), SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Props.Data.Bytes.Generators qualified as Gens
import Props.Data.Size.Generators qualified as SGens
import Props.MaxRuns (MaxRuns (..))
import Props.Verify.Algebra qualified as VAlgebra
import Props.Verify.Conversion (ResultConvs (..))
import Props.Verify.Conversion qualified as VConversion
import Props.Verify.Normalize qualified as VNormalize
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
  [ convertProps,
    incProps,
    decProps,
    normalizeProps,
    bytesEqProps,
    bytesOrdProps,
    bytesGroupProps,
    bytesVectorSpaceProps
  ]

anySizeProps :: [TestTree]
anySizeProps =
  [ anySizeEqProps,
    anySizeOrdProps,
    anySizeGroupProps,
    anyVectorSpaceProps,
    anyNormalizeProps
  ]

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Conversions" $
    H.withTests limit $
      H.property $ do
        b <- H.forAll (Gens.genBytes @'B)
        k <- H.forAll (Gens.genBytes @'K)
        m <- H.forAll (Gens.genBytes @'M)
        g <- H.forAll (Gens.genBytes @'G)
        t <- H.forAll (Gens.genBytes @'T)
        p <- H.forAll (Gens.genBytes @'P)
        convert b VConversion.convertB
        convert k VConversion.convertK
        convert m VConversion.convertM
        convert g VConversion.convertG
        convert t VConversion.convertT
        convert p VConversion.convertP

convert ::
  SingSize s =>
  Bytes s Rational ->
  (ResultConvs Rational -> PropertyT IO ()) ->
  PropertyT IO ()
convert bytes@(MkBytes x) convertAndTestFn = do
  let original = x
      bRes = Bytes.unBytes $ toB bytes
      kRes = Bytes.unBytes $ toK bytes
      mRes = Bytes.unBytes $ toM bytes
      gRes = Bytes.unBytes $ toG bytes
      tRes = Bytes.unBytes $ toT bytes
      pRes = Bytes.unBytes $ toP bytes
  convertAndTestFn MkResultConvs {..}

incProps :: TestTree
incProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes increasing label reduces size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkAnySize sz bytes@(MkBytes x)) <- H.forAll Gens.genNormalizedBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SP -> (x, Bytes.unBytes bytes)
              SB -> Size.withSingSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              SK -> Size.withSingSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              SM -> Size.withSingSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              SG -> Size.withSingSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
              ST -> Size.withSingSize sz (x .%. 1_000, Bytes.unBytes (next bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected

decProps :: TestTree
decProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes decreasing label multiplies size by 1,000" $
    H.withTests limit $
      H.property $ do
        (MkAnySize sz bytes@(MkBytes x)) <- H.forAll Gens.genNormalizedBytes
        let (expected, result) :: (Rational, Rational) = case sz of
              SB -> (x, Bytes.unBytes bytes)
              SK -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SM -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SG -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              ST -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
              SP -> Size.withSingSize sz (x .*. 1_000, Bytes.unBytes (prev bytes))
        H.footnote $ "expected: " <> show expected
        H.footnote $ " result: " <> show result
        result === expected

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes normalizes" $
    H.withTests limit $
      H.property $ do
        (MkAnySize sz bytes) <- H.forAll Gens.genSomeBytes
        let normalized@(MkAnySize _ (MkBytes x)) = Size.withSingSize sz $ normalize bytes
            label = anySizeToLabel normalized
        H.footnote $ "original: " <> show bytes
        H.footnote $ "normalized: " <> show normalized
        VNormalize.isNormalized label x

bytesEqProps :: TestTree
bytesEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.eqLaws x y z

bytesOrdProps :: TestTree
bytesOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.ordLaws x y z

bytesGroupProps :: TestTree
bytesGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.groupLaws x y z

bytesVectorSpaceProps :: TestTree
bytesVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Bytes Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        k <- H.forAll SGens.genD
        l <- H.forAll SGens.genD
        VAlgebra.vectorSpaceLaws x y k l

anySizeToLabel :: AnySize n -> Size
anySizeToLabel (MkAnySize sz _) = case sz of
  SB -> B
  SK -> K
  SM -> M
  SG -> G
  ST -> T
  SP -> P

anySizeEqProps :: TestTree
anySizeEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.eqLaws x y z

anySizeOrdProps :: TestTree
anySizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.ordLaws x y z

anySizeGroupProps :: TestTree
anySizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.groupLaws x y z

anyVectorSpaceProps :: TestTree
anyVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        k <- H.forAll SGens.genD
        l <- H.forAll SGens.genD
        VAlgebra.vectorSpaceLaws x y k l

anyNormalizeProps :: TestTree
anyNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnySize normalization laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        k <- H.forAll SGens.genD
        VNormalize.normalizeLaws x y k
