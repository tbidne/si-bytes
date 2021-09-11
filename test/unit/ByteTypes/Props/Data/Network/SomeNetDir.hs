{-# LANGUAGE RecordWildCards #-}

-- | Property tests for 'SomeNetDir'.
module ByteTypes.Props.Data.Network.SomeNetDir (props) where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Data.Network.NetBytes qualified as NetBytes
import ByteTypes.Data.Network.NetBytes.Internal (NetBytes (..), SomeNetSize (..))
import ByteTypes.Data.Network.SomeNetDir.Internal (SomeNet (..), SomeNetDir (..))
import ByteTypes.Data.Size (SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import ByteTypes.Props.Data.Network.Generators qualified as NGens
import ByteTypes.Props.MaxRuns (MaxRuns (..))
import ByteTypes.Props.Verify.Algebra qualified as VAlgebra
import ByteTypes.Props.Verify.Conversion (ResultConvs (..))
import ByteTypes.Props.Verify.Conversion qualified as VConversion
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Network.SomeNetDir"
    $ someNetDirBytesProps <> someNetBytesProps

someNetDirBytesProps :: [TestTree]
someNetDirBytesProps =
  [ convertSomeNetDirProps,
    normalizeSomeNetDirProps,
    someNetDirEqProps
  ]

someNetBytesProps :: [TestTree]
someNetBytesProps =
  [ convertSomeNetProps,
    normalizeSomeNetProps,
    someNetEqProps
  ]

convertSomeNetDirProps :: TestTree
convertSomeNetDirProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetDir Conversions" $
    H.withTests limit $
      H.property $ do
        b <- H.forAll (NGens.genSomeNetDirUp @'B)
        k <- H.forAll (NGens.genSomeNetDirUp @'K)
        m <- H.forAll (NGens.genSomeNetDirUp @'M)
        g <- H.forAll (NGens.genSomeNetDirUp @'G)
        t <- H.forAll (NGens.genSomeNetDirUp @'T)
        p <- H.forAll (NGens.genSomeNetDirUp @'P)
        convert b VConversion.convertB
        convert k VConversion.convertK
        convert m VConversion.convertM
        convert g VConversion.convertG
        convert t VConversion.convertT
        convert p VConversion.convertP

convert ::
  SingSize s =>
  SomeNetDir s Rational ->
  (ResultConvs Rational -> PropertyT IO ()) ->
  PropertyT IO ()
convert bytes@(MkSomeNetDir _ (MkNetBytesP x)) convertAndTestFn = do
  let original = x
      (MkSomeNetDir _ (MkNetBytesP bRes)) = toB bytes
      (MkSomeNetDir _ (MkNetBytesP kRes)) = toK bytes
      (MkSomeNetDir _ (MkNetBytesP mRes)) = toM bytes
      (MkSomeNetDir _ (MkNetBytesP gRes)) = toG bytes
      (MkSomeNetDir _ (MkNetBytesP tRes)) = toT bytes
      (MkSomeNetDir _ (MkNetBytesP pRes)) = toP bytes
  convertAndTestFn MkResultConvs {..}

normalizeSomeNetDirProps :: TestTree
normalizeSomeNetDirProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetDir normalizes matches NetBytes" $
    H.withTests limit $
      H.property $ do
        (MkSomeNet dir szx originalBytes) <- H.forAll NGens.genSomeNet
        let someNetDir = MkSomeNetDir dir originalBytes
            someNetDirNorm = case Size.withSingSize szx $ normalize someNetDir of
              MkSomeNet _ _ netDirBytes -> NetBytes.unNetBytesP netDirBytes
            netNorm = case Size.withSingSize szx $ normalize originalBytes of
              MkSomeNetSize _ netBytes -> NetBytes.unNetBytesP netBytes
        someNetDirNorm === netNorm

someNetDirEqProps :: TestTree
someNetDirEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetDir Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll $ NGens.genSomeNetDirDown @'P
        y <- H.forAll $ NGens.genSomeNetDirDown @'P
        z <- H.forAll $ NGens.genSomeNetDirDown @'P
        VAlgebra.eqLaws x y z

convertSomeNetProps :: TestTree
convertSomeNetProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNetDir Conversions" $
    H.withTests limit $
      H.property $ do
        someSize@(MkSomeNet dir sz bytes) <- H.forAll NGens.genSomeNet
        toB someSize === MkSomeNetDir dir (Size.withSingSize sz (toB bytes))
        toK someSize === MkSomeNetDir dir (Size.withSingSize sz (toK bytes))
        toM someSize === MkSomeNetDir dir (Size.withSingSize sz (toM bytes))
        toG someSize === MkSomeNetDir dir (Size.withSingSize sz (toG bytes))
        toT someSize === MkSomeNetDir dir (Size.withSingSize sz (toT bytes))
        toP someSize === MkSomeNetDir dir (Size.withSingSize sz (toP bytes))

normalizeSomeNetProps :: TestTree
normalizeSomeNetProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNet normalizes matches NetBytes" $
    H.withTests limit $
      H.property $ do
        someNet@(MkSomeNet _ szx originalBytes) <- H.forAll NGens.genSomeNet
        let someNetDirNorm = case Size.withSingSize szx $ normalize someNet of
              MkSomeNet _ _ netDirBytes -> NetBytes.unNetBytesP netDirBytes
            netNorm = case Size.withSingSize szx $ normalize originalBytes of
              MkSomeNetSize _ netBytes -> NetBytes.unNetBytesP netBytes
        someNetDirNorm === netNorm

someNetEqProps :: TestTree
someNetEqProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "SomeNet Eq laws" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNet
        y <- H.forAll NGens.genSomeNet
        z <- H.forAll NGens.genSomeNet
        VAlgebra.eqLaws x y z
