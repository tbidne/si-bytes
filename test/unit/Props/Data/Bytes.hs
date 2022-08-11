-- | Property tests for 'Bytes'.
module Props.Data.Bytes (props) where

import Control.Monad (join)
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Internal (Bytes (..), SomeSize (..))
import Data.Bytes.Size (SSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Hedgehog ((===))
import Hedgehog qualified as H
import Props.Generators.Bytes qualified as Gens
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
    "Bytes.Data.Bytes"
    [ bytesProps,
      someSizeProps
    ]

bytesProps :: TestTree
bytesProps =
  T.testGroup
    "Bytes"
    [ unBytesProps,
      convertProps,
      normalizeProps,
      bytesEqProps,
      bytesOrdProps,
      bytesGroupProps,
      bytesVectorSpaceProps
    ]

someSizeProps :: TestTree
someSizeProps =
  T.testGroup
    "SomeSize"
    [ someConvertProps,
      someSizeEqProps,
      someSizeOrdProps,
      someSizeGroupProps,
      someVectorSpaceProps,
      someNormalizeProps
    ]

unBytesProps :: TestTree
unBytesProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes unwrapping + wrap is a no-op" "unBytesProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize _ bytes) <- H.forAll Gens.genSomeBytes
        bytes === MkBytes (unwrap bytes)

convertProps :: TestTree
convertProps =
  T.testGroup
    "Conversions"
    $ join
      [ VConv.testConvertToAll (Gens.genBytes @B) VConv.expectedB "B",
        VConv.testConvertToAll (Gens.genBytes @K) VConv.expectedK "K",
        VConv.testConvertToAll (Gens.genBytes @M) VConv.expectedM "M",
        VConv.testConvertToAll (Gens.genBytes @G) VConv.expectedG "G",
        VConv.testConvertToAll (Gens.genBytes @T) VConv.expectedT "T",
        VConv.testConvertToAll (Gens.genBytes @P) VConv.expectedP "P",
        VConv.testConvertToAll (Gens.genBytes @E) VConv.expectedE "E",
        VConv.testConvertToAll (Gens.genBytes @Z) VConv.expectedZ "Z",
        VConv.testConvertToAll (Gens.genBytes @Y) VConv.expectedY "Y"
      ]

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes normalizes" "normalizeProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize sz bytes) <- H.forAll Gens.genSomeBytes
        let normalized@(MkSomeSize _ (MkBytes x)) = Size.withSingSize sz $ normalize bytes
            label = someSizeToLabel normalized
        H.footnote $ "original: " <> show bytes
        H.footnote $ "normalized: " <> show normalized
        VNormalize.isNormalized label x

bytesEqProps :: TestTree
bytesEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Eq laws" "bytesEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.eqLaws x y z

bytesOrdProps :: TestTree
bytesOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Ord laws" "bytesOrdProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.ordLaws x y z

bytesGroupProps :: TestTree
bytesGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Group laws" "bytesGroupProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        z <- H.forAll (Gens.genBytes @'P)
        VAlgebra.groupLaws x y z

bytesVectorSpaceProps :: TestTree
bytesVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes Vector Space laws" "bytesVectorSpaceProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll (Gens.genBytes @'P)
        y <- H.forAll (Gens.genBytes @'P)
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someConvertProps :: TestTree
someConvertProps =
  T.testGroup
    "Conversions"
    $ join
      [ VConv.testConvertToAll (Gens.genSomeSizeFromSSize SB) VConv.expectedB "B",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize SK) VConv.expectedK "K",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize SM) VConv.expectedM "M",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize SG) VConv.expectedG "G",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize ST) VConv.expectedT "T",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize SP) VConv.expectedP "P",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize SE) VConv.expectedE "E",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize SZ) VConv.expectedZ "Z",
        VConv.testConvertToAll (Gens.genSomeSizeFromSSize SY) VConv.expectedY "Y"
      ]

someSizeToLabel :: SomeSize n -> Size
someSizeToLabel (MkSomeSize sz _) = case sz of
  SB -> B
  SK -> K
  SM -> M
  SG -> G
  ST -> T
  SP -> P
  SE -> E
  SZ -> Z
  SY -> Y

someSizeEqProps :: TestTree
someSizeEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Eq laws" "someSizeEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.eqLaws x y z

someSizeOrdProps :: TestTree
someSizeOrdProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Ord laws" "someSizeOrdProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.ordLaws x y z

someSizeGroupProps :: TestTree
someSizeGroupProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Group laws" "someSizeGroupProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        z <- H.forAll Gens.genSomeBytes
        VAlgebra.groupLaws x y z

someVectorSpaceProps :: TestTree
someVectorSpaceProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize Vector Space laws" "someVectorSpaceProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        k <- H.forAll SGens.genNonZero
        l <- H.forAll SGens.genNonZero
        VAlgebra.vectorSpaceLaws x y k l

someNormalizeProps :: TestTree
someNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize normalization" "someNormalizeProps" $
    H.withTests limit $
      H.property $ do
        x@(MkSomeSize szx bytes) <- H.forAll Gens.genSomeBytes
        y <- H.forAll Gens.genSomeBytes
        k <- H.forAll SGens.genD
        nz <- H.forAll SGens.genNonZero
        -- matches underlying bytes
        normalize x === Size.withSingSize szx (normalize bytes)
        -- laws
        VNormalize.normalizeLaws x y k nz
