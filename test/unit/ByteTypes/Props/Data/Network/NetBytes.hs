{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'NetBytes'.
module ByteTypes.Props.Data.Network.NetBytes (props) where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Data.Bytes (Bytes (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Direction (Direction (..))
import ByteTypes.Data.Network.NetBytes (NetBytes (..), SomeNetSize (..))
import ByteTypes.Data.Network.NetBytes qualified as NetBytes
import ByteTypes.Data.Size (SSize (..), SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import ByteTypes.Props.Data.Network.Generators qualified as NGens
import ByteTypes.Props.Data.Size.Generators qualified as SGens
import ByteTypes.Props.MaxRuns (MaxRuns (..))
import ByteTypes.Props.Verify.Algebra qualified as VAlgebra
import ByteTypes.Props.Verify.Conversion (ResultConvs (..))
import ByteTypes.Props.Verify.Conversion qualified as VConversion
import ByteTypes.Props.Verify.Normalize qualified as VNormalize
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Network.NetBytes"
    $ netBytesProps <> someNetSizeProps

netBytesProps :: [TestTree]
netBytesProps =
  [ unNetBytesProps,
    convertProps,
    normalizeProps,
    netBytesEqProps,
    netBytesOrdProps,
    netBytesGroupProps,
    netBytesVectorSpaceProps
  ]

someNetSizeProps :: [TestTree]
someNetSizeProps =
  [ someConvertProps,
    someNetSizeEqProps,
    someNetSizeOrdProps,
    someNetSizeGroupProps,
    someNetSizeVectorSpaceProps,
    someNetSizeNormalizeProps
  ]

unNetBytesProps :: TestTree
unNetBytesProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "NetBytes unwrapping + wrap is a no-op" $
    H.withTests limit $
      H.property $ do
        (MkSomeNetSize _ bytes) <- H.forAll NGens.genSomeNetSizeUp
        bytes === MkNetBytes (NetBytes.unNetBytes bytes)

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
        (MkSomeNetSize sz bytes) <- H.forAll NGens.genSomeNetSizeUp
        let normalized@(MkSomeNetSize _ (MkNetBytes (MkBytes x))) =
              Size.withSingSize sz $ normalize bytes
            label = someSizeToLabel normalized
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
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someConvertProps :: TestTree
someConvertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetSize conversions match underlying NetBytes" $
    H.withTests limit $
      H.property $ do
        someSize@(MkSomeNetSize sz bytes) <- H.forAll NGens.genSomeNetSizeUp
        toB someSize === Size.withSingSize sz (toB bytes)
        toK someSize === Size.withSingSize sz (toK bytes)
        toM someSize === Size.withSingSize sz (toM bytes)
        toG someSize === Size.withSingSize sz (toG bytes)
        toT someSize === Size.withSingSize sz (toT bytes)
        toP someSize === Size.withSingSize sz (toP bytes)

someNetSizeEqProps :: TestTree
someNetSizeEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetSize Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.eqLaws x y z

someNetSizeOrdProps :: TestTree
someNetSizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetSize Ord laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.ordLaws x y z

someNetSizeGroupProps :: TestTree
someNetSizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetSize Group laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.groupLaws x y z

someNetSizeVectorSpaceProps :: TestTree
someNetSizeVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetSize Vector Space laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someNetSizeNormalizeProps :: TestTree
someNetSizeNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetSize normalization" $
    H.withTests limit $
      H.property $ do
        x@(MkSomeNetSize szx bytes) <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        k <- H.forAll SGens.genD
        nz <- H.forAll SGens.genNonZero
        -- matches underlying bytes
        normalize x === Size.withSingSize szx (normalize bytes)
        -- laws
        VNormalize.normalizeLaws x y k nz

someSizeToLabel :: SomeNetSize d n -> Size
someSizeToLabel (MkSomeNetSize sz _) = case sz of
  SB -> B
  SK -> K
  SM -> M
  SG -> G
  ST -> T
  SP -> P
