-- | Property tests for Network.
module Unit.Data.Bytes.Network (props) where

import Control.Monad (join)
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Network.Direction (Direction (..))
import Data.Bytes.Network.Internal
  ( NetBytes (..),
    SomeNet (..),
    SomeNetDir (..),
    SomeNetSize (..),
  )
import Data.Bytes.Size (SSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.Generators.Network qualified as NGens
import Unit.Props.Generators.Size qualified as SGens
import Unit.Props.MaxRuns (MaxRuns (..))
import Unit.Props.Utils qualified as U
import Unit.Props.Verify.Algebra qualified as VAlgebra
import Unit.Props.Verify.Conversion qualified as VConv
import Unit.Props.Verify.Normalize qualified as VNormalize

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Network"
    [ netBytesProps,
      someNetSizeProps,
      someNetDirProps,
      someNetProps
    ]

netBytesProps :: TestTree
netBytesProps =
  T.testGroup
    "NetBytes"
    [ unNetBytesProps,
      convertProps,
      normalizeProps,
      netBytesEqProps,
      netBytesOrdProps,
      netBytesGroupProps,
      netBytesVectorSpaceProps
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

someNetSizeProps :: TestTree
someNetSizeProps =
  T.testGroup
    "SomeNetSize"
    [ someConvertProps,
      someNetSizeEqProps,
      someNetSizeOrdProps,
      someNetSizeGroupProps,
      someNetSizeVectorSpaceProps,
      someNetSizeNormalizeProps
    ]

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

someNetDirProps :: TestTree
someNetDirProps =
  T.testGroup
    "SomeNetDir"
    [ convertSomeNetDirProps,
      normalizeSomeNetDirProps,
      someNetDirEqProps
    ]

convertSomeNetDirProps :: TestTree
convertSomeNetDirProps =
  T.testGroup
    "Conversions"
    $ join
      [ VConv.testConvertToAll (NGens.genSomeNetDirUp @B) VConv.expectedB "B",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @K) VConv.expectedK "K",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @M) VConv.expectedM "M",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @G) VConv.expectedG "G",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @T) VConv.expectedT "T",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @P) VConv.expectedP "P",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @E) VConv.expectedE "E",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @Z) VConv.expectedZ "Z",
        VConv.testConvertToAll (NGens.genSomeNetDirUp @Y) VConv.expectedY "Y"
      ]

normalizeSomeNetDirProps :: TestTree
normalizeSomeNetDirProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetDir normalizes matches NetBytes" "normalizeSomeNetDirProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeNet dir szx originalBytes) <- H.forAll NGens.genSomeNet
        let someNetDir = MkSomeNetDir dir originalBytes
            someNetDirNorm = case Size.withSingSize szx $ normalize someNetDir of
              MkSomeNet _ _ netDirBytes -> unwrap netDirBytes
            netNorm = case Size.withSingSize szx $ normalize originalBytes of
              MkSomeNetSize _ netBytes -> unwrap netBytes
        someNetDirNorm === netNorm

someNetDirEqProps :: TestTree
someNetDirEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNetDir Eq laws" "someNetDirEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll $ NGens.genSomeNetDirDown @'P
        y <- H.forAll $ NGens.genSomeNetDirDown @'P
        z <- H.forAll $ NGens.genSomeNetDirDown @'P
        VAlgebra.eqLaws x y z

someNetProps :: TestTree
someNetProps =
  T.testGroup
    "SomeNet"
    [ convertSomeNetProps,
      normalizeSomeNetProps,
      someNetEqProps
    ]

convertSomeNetProps :: TestTree
convertSomeNetProps =
  T.testGroup
    "Conversions"
    $ join
      [ VConv.testConvertToAll (NGens.genSomeNetFromSSize SB) VConv.expectedB "B",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize SK) VConv.expectedK "K",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize SM) VConv.expectedM "M",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize SG) VConv.expectedG "G",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize ST) VConv.expectedT "T",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize SP) VConv.expectedP "P",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize SE) VConv.expectedE "E",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize SZ) VConv.expectedZ "Z",
        VConv.testConvertToAll (NGens.genSomeNetFromSSize SY) VConv.expectedY "Y"
      ]

normalizeSomeNetProps :: TestTree
normalizeSomeNetProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNet normalizes matches NetBytes" "normalizeSomeNetProps" $
    H.withTests limit $
      H.property $ do
        someNet@(MkSomeNet _ szx originalBytes) <- H.forAll NGens.genSomeNet
        let someNetDirNorm = case Size.withSingSize szx $ normalize someNet of
              MkSomeNet _ _ netDirBytes -> unwrap netDirBytes
            netNorm = case Size.withSingSize szx $ normalize originalBytes of
              MkSomeNetSize _ netBytes -> unwrap netBytes
        someNetDirNorm === netNorm

someNetEqProps :: TestTree
someNetEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNet Eq laws" "someNetEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNet
        y <- H.forAll NGens.genSomeNet
        z <- H.forAll NGens.genSomeNet
        VAlgebra.eqLaws x y z
