{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'NetBytes'.
module Props.Data.Bytes.Network.NetBytes (props) where

import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Network.Direction (Direction (..))
import Data.Bytes.Network.NetBytes.Internal (NetBytes (..), SomeNetSize (..))
import Data.Bytes.Size (SSize (..), SingSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Props.Generators.Network qualified as NGens
import Props.Generators.Size qualified as SGens
import Props.MaxRuns (MaxRuns (..))
import Props.Utils qualified as U
import Props.Verify.Algebra qualified as VAlgebra
import Props.Verify.Conversion (ResultConvs (..))
import Props.Verify.Conversion qualified as VConversion
import Props.Verify.Normalize qualified as VNormalize
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

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
  U.testPropertyCompat "NetBytes unwrapping + wrap is a no-op" "unNetBytesProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeNetSize _ bytes) <- H.forAll NGens.genSomeNetSizeUp
        bytes === MkNetBytesP (unwrap bytes)

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "NetBytes Conversions" "convertProps" $
    H.withTests limit $
      H.property $ do
        b <- H.forAll (NGens.genNet @'Up @'B)
        k <- H.forAll (NGens.genNet @'Up @'K)
        m <- H.forAll (NGens.genNet @'Up @'M)
        g <- H.forAll (NGens.genNet @'Up @'G)
        t <- H.forAll (NGens.genNet @'Up @'T)
        p <- H.forAll (NGens.genNet @'Up @'P)
        e <- H.forAll (NGens.genNet @'Up @'E)
        z <- H.forAll (NGens.genNet @'Up @'Z)
        y <- H.forAll (NGens.genNet @'Up @'Y)
        convert b VConversion.convertB
        convert k VConversion.convertK
        convert m VConversion.convertM
        convert g VConversion.convertG
        convert t VConversion.convertT
        convert p VConversion.convertP
        convert e VConversion.convertE
        convert z VConversion.convertZ
        convert y VConversion.convertY

convert ::
  SingSize s =>
  NetBytes d s Rational ->
  (ResultConvs Rational -> PropertyT IO ()) ->
  PropertyT IO ()
convert bytes@(MkNetBytesP x) convertAndTestFn = do
  let original = x
      bRes = unwrap $ toB bytes
      kRes = unwrap $ toK bytes
      mRes = unwrap $ toM bytes
      gRes = unwrap $ toG bytes
      tRes = unwrap $ toT bytes
      pRes = unwrap $ toP bytes
      eRes = unwrap $ toE bytes
      zRes = unwrap $ toZ bytes
      yRes = unwrap $ toY bytes
  convertAndTestFn MkResultConvs {..}

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "NetBytes normalizes" "normalizeProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeNetSize sz bytes) <- H.forAll NGens.genSomeNetSizeUp
        let normalized@(MkSomeNetSize _ (MkNetBytesP x)) =
              Size.withSingSize sz $ normalize bytes
            label = someSizeToLabel normalized
        H.footnote $ "original: " <> show bytes
        H.footnote $ "normalized: " <> show normalized
        VNormalize.isNormalized label x

netBytesEqProps :: TestTree
netBytesEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "NetBytes Eq laws" "netBytesEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        z <- H.forAll (NGens.genNet @'Up @'P)
        VAlgebra.eqLaws x y z

netBytesOrdProps :: TestTree
netBytesOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "NetBytes Ord laws" "netBytesOrdProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        z <- H.forAll (NGens.genNet @'Up @'P)
        VAlgebra.ordLaws x y z

netBytesGroupProps :: TestTree
netBytesGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "NetBytes Group laws" "netBytesGroupProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        z <- H.forAll (NGens.genNet @'Up @'P)
        VAlgebra.groupLaws x y z

netBytesVectorSpaceProps :: TestTree
netBytesVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "NetBytes Vector Space laws" "netBytesVectorSpaceProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (NGens.genNet @'Up @'P)
        y <- H.forAll (NGens.genNet @'Up @'P)
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someConvertProps :: TestTree
someConvertProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetSize conversions match underlying NetBytes" "someConvertProps" $
    H.withTests limit $
      H.property $ do
        someSize@(MkSomeNetSize sz bytes) <- H.forAll NGens.genSomeNetSizeUp
        toB someSize === Size.withSingSize sz (toB bytes)
        toK someSize === Size.withSingSize sz (toK bytes)
        toM someSize === Size.withSingSize sz (toM bytes)
        toG someSize === Size.withSingSize sz (toG bytes)
        toT someSize === Size.withSingSize sz (toT bytes)
        toP someSize === Size.withSingSize sz (toP bytes)
        toE someSize === Size.withSingSize sz (toE bytes)
        toZ someSize === Size.withSingSize sz (toZ bytes)
        toY someSize === Size.withSingSize sz (toY bytes)

someNetSizeEqProps :: TestTree
someNetSizeEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetSize Eq laws" "someNetSizeEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.eqLaws x y z

someNetSizeOrdProps :: TestTree
someNetSizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetSize Ord laws" "someNetSizeOrdProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.ordLaws x y z

someNetSizeGroupProps :: TestTree
someNetSizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetSize Group laws" "someNetSizeGroupProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        z <- H.forAll NGens.genSomeNetSizeUp
        VAlgebra.groupLaws x y z

someNetSizeVectorSpaceProps :: TestTree
someNetSizeVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetSize Vector Space laws" "someNetSizeVectorSpaceProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNetSizeUp
        y <- H.forAll NGens.genSomeNetSizeUp
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someNetSizeNormalizeProps :: TestTree
someNetSizeNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetSize normalization" "someNetSizeNormalizeProps" $
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
  SE -> E
  SZ -> Z
  SY -> Y
