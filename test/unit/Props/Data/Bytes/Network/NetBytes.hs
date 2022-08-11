-- | Property tests for 'NetBytes'.
module Props.Data.Bytes.Network.NetBytes (props) where

import Control.Monad (join)
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Network.Direction (Direction (..))
import Data.Bytes.Network.NetBytes.Internal (NetBytes (..), SomeNetSize (..))
import Data.Bytes.Size (SSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Hedgehog ((===))
import Hedgehog qualified as H
import Props.Generators.Network qualified as NGens
import Props.Generators.Size qualified as SGens
import Props.MaxRuns (MaxRuns (..))
import Props.Utils qualified as U
import Props.Verify.Algebra qualified as VAlgebra
import Props.Verify.Conversion qualified as VConv
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
convertProps =
  T.testGroup
    "Conversions"
    $ join
      [ VConv.testConvertToAll (NGens.genNet @'Up @'B) VConv.expectedB "B",
        VConv.testConvertToAll (NGens.genNet @'Up @'K) VConv.expectedK "K",
        VConv.testConvertToAll (NGens.genNet @'Up @'M) VConv.expectedM "M",
        VConv.testConvertToAll (NGens.genNet @'Up @'G) VConv.expectedG "G",
        VConv.testConvertToAll (NGens.genNet @'Up @'T) VConv.expectedT "T",
        VConv.testConvertToAll (NGens.genNet @'Up @'P) VConv.expectedP "P",
        VConv.testConvertToAll (NGens.genNet @'Up @'E) VConv.expectedE "E",
        VConv.testConvertToAll (NGens.genNet @'Up @'Z) VConv.expectedZ "Z",
        VConv.testConvertToAll (NGens.genNet @'Up @'Y) VConv.expectedY "Y"
      ]

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
someConvertProps =
  T.testGroup
    "Conversions"
    $ join
      [ VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SB) VConv.expectedB "B",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SK) VConv.expectedK "K",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SM) VConv.expectedM "M",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SG) VConv.expectedG "G",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize ST) VConv.expectedT "T",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SP) VConv.expectedP "P",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SE) VConv.expectedE "E",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SZ) VConv.expectedZ "Z",
        VConv.testConvertToAll (NGens.genSomeNetSizeUpFromSSize SY) VConv.expectedY "Y"
      ]

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
