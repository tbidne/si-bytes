{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'NetBytes'.
module Props.Data.Network.NetBytes (props) where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Data.Bytes (Bytes (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Direction (Direction (..))
import ByteTypes.Data.Network.NetBytes (AnyNetSize (..), NetBytes (..))
import ByteTypes.Data.Network.NetBytes qualified as NetBytes
import ByteTypes.Data.Size (SSize (..), SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import Hedgehog (PropertyT)
import Hedgehog qualified as H
import Props.Data.Network.Generators qualified as NGens
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
    "Bytes.Data.Network.NetBytes"
    $ netBytesProps <> anyNetSizeProps

netBytesProps :: [TestTree]
netBytesProps =
  [ convertProps,
    normalizeProps,
    netBytesEqProps,
    netBytesOrdProps,
    netBytesGroupProps,
    netBytesVectorSpaceProps
  ]

anyNetSizeProps :: [TestTree]
anyNetSizeProps =
  [ anyNetSizeEqProps,
    anyNetSizeOrdProps,
    anyNetSizeGroupProps,
    anyNetSizeVectorSpaceProps,
    anyNetSizeNormalizeProps
  ]

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes Conversions" $
    H.withTests limit $
      H.property $ do
        b <- H.forAll (NGens.genNet @'Up @'B)
        k <- H.forAll (NGens.genNet @'Up @'K)
        m <- H.forAll (NGens.genNet @'Up @'M)
        g <- H.forAll (NGens.genNet @'Up @'G)
        t <- H.forAll (NGens.genNet @'Up @'T)
        p <- H.forAll (NGens.genNet @'Up @'P)
        convert b VConversion.convertB
        convert k VConversion.convertK
        convert m VConversion.convertM
        convert g VConversion.convertG
        convert t VConversion.convertT
        convert p VConversion.convertP

convert ::
  SingSize s =>
  NetBytes d s Rational ->
  (ResultConvs Rational -> PropertyT IO ()) ->
  PropertyT IO ()
convert bytes@(MkNetBytes (MkBytes x)) convertAndTestFn = do
  let original = x
      bRes = Bytes.unBytes $ NetBytes.unNetBytes $ toB bytes
      kRes = Bytes.unBytes $ NetBytes.unNetBytes $ toK bytes
      mRes = Bytes.unBytes $ NetBytes.unNetBytes $ toM bytes
      gRes = Bytes.unBytes $ NetBytes.unNetBytes $ toG bytes
      tRes = Bytes.unBytes $ NetBytes.unNetBytes $ toT bytes
      pRes = Bytes.unBytes $ NetBytes.unNetBytes $ toP bytes
  convertAndTestFn MkResultConvs {..}

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes normalizes" $
    H.withTests limit $
      H.property $ do
        (MkAnyNetSize sz bytes) <- H.forAll NGens.genSomeNetSizeUp
        let normalized@(MkAnyNetSize _ (MkNetBytes (MkBytes x))) =
              Size.withSingSize sz $ normalize bytes
            label = anySizeToLabel normalized
        H.footnote $ "original: " <> show bytes
        H.footnote $ "normalized: " <> show normalized
        VNormalize.isNormalized label x

netBytesEqProps :: TestTree
netBytesEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        z <- H.forAll (NGens.genNet @'Up @'P)
        VAlgebra.eqLaws x y z

netBytesOrdProps :: TestTree
netBytesOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        z <- H.forAll (NGens.genNet @'Up @'P)
        VAlgebra.ordLaws x y z

netBytesGroupProps :: TestTree
netBytesGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        z <- H.forAll (NGens.genNet @'Up @'P)
        VAlgebra.groupLaws x y z

netBytesVectorSpaceProps :: TestTree
netBytesVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        k <- H.forAll SGens.genD
        l <- H.forAll SGens.genD
        VAlgebra.vectorSpaceLaws x y k l

anyNetSizeEqProps :: TestTree
anyNetSizeEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnyNetSize Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.eqLaws x y z

anyNetSizeOrdProps :: TestTree
anyNetSizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnyNetSize Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.ordLaws x y z

anyNetSizeGroupProps :: TestTree
anyNetSizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnyNetSize Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.groupLaws x y z

anyNetSizeVectorSpaceProps :: TestTree
anyNetSizeVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnyNetSize Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        k <- H.forAll SGens.genD
        l <- H.forAll SGens.genD
        VAlgebra.vectorSpaceLaws x y k l

anyNetSizeNormalizeProps :: TestTree
anyNetSizeNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "AnyNetSize normalization laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        k <- H.forAll SGens.genD
        VNormalize.normalizeLaws x y k

anySizeToLabel :: AnyNetSize d n -> Size
anySizeToLabel (MkAnyNetSize sz _) = case sz of
  SB -> B
  SK -> K
  SM -> M
  SG -> G
  ST -> T
  SP -> P
