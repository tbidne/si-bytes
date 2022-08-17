-- | Property tests for 'Bytes'.
module Unit.Data.Bytes (tests) where

import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting qualified as Formatting
import Data.Bytes.Internal (Bytes (..), SomeSize (..))
import Data.Bytes.Size (SSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.Generators.Bytes qualified as Gens
import Unit.Props.Generators.Formatting qualified as FGens
import Unit.Props.Generators.Size qualified as SGens
import Unit.Props.MaxRuns (MaxRuns (..))
import Unit.Props.Verify.Algebra qualified as VAlgebra
import Unit.Props.Verify.Conversion qualified as VConv
import Unit.Props.Verify.Normalize qualified as VNormalize
import Unit.Props.Verify.Parsing qualified as VParsing
import Unit.Utils qualified as U

-- | @since 0.1.
tests :: TestTree
tests =
  T.testGroup
    "Data.Bytes"
    [ bytesTests,
      someSizeTests
    ]

bytesTests :: TestTree
bytesTests =
  T.testGroup
    "Bytes"
    [ unBytesProps,
      convertProps,
      U.convGoldens "bytes" (MkBytes @B) (MkBytes @Y),
      normalizeProps,
      normalizeGoldens,
      algebraTests
    ]

algebraTests :: TestTree
algebraTests =
  T.testGroup
    "Algebra"
    [ bytesEqProps,
      bytesOrdProps,
      bytesGroupProps,
      bytesVectorSpaceProps
    ]

someSizeTests :: TestTree
someSizeTests =
  T.testGroup
    "SomeSize"
    [ U.convGoldens "some-size" (MkBytes @B) (MkBytes @Y),
      someNormalizeGoldens,
      someParsingTests,
      someAlgebraTests
    ]

someParsingTests :: TestTree
someParsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsingRoundTrip genBytes genFmt mkFmt
    ]
  where
    mkFmt = Formatting.formatSized baseFmt
    baseFmt = Formatting.MkFloatingFormatter (Just 2)
    genBytes = Gens.genSomeBytesFloating @Double
    genFmt = FGens.genSizedFormatter

someAlgebraTests :: TestTree
someAlgebraTests =
  T.testGroup
    "Algebra"
    [ someSizeEqProps,
      someSizeOrdProps,
      someSizeGroupProps,
      someVectorSpaceProps
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
    "Conversion Properties"
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

normalizeGoldens :: TestTree
normalizeGoldens = T.testGroup "Normalize Goldens" tests'
  where
    tests' =
      [ U.normGoldensForUnit "bytes" 'B' (MkBytes @B),
        U.normGoldensForUnit "bytes" 'K' (MkBytes @K),
        U.normGoldensForUnit "bytes" 'M' (MkBytes @M),
        U.normGoldensForUnit "bytes" 'G' (MkBytes @G),
        U.normGoldensForUnit "bytes" 'T' (MkBytes @T),
        U.normGoldensForUnit "bytes" 'P' (MkBytes @P),
        U.normGoldensForUnit "bytes" 'E' (MkBytes @E),
        U.normGoldensForUnit "bytes" 'Z' (MkBytes @Z),
        U.normGoldensForUnit "bytes" 'Y' (MkBytes @Y)
      ]

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

someNormalizeGoldens :: TestTree
someNormalizeGoldens = T.testGroup "Normalize Goldens" tests'
  where
    tests' =
      [ U.normGoldensForUnit "some-size" 'B' (MkSomeSize SB . MkBytes),
        U.normGoldensForUnit "some-size" 'K' (MkSomeSize SK . MkBytes),
        U.normGoldensForUnit "some-size" 'M' (MkSomeSize SM . MkBytes),
        U.normGoldensForUnit "some-size" 'G' (MkSomeSize SG . MkBytes),
        U.normGoldensForUnit "some-size" 'T' (MkSomeSize ST . MkBytes),
        U.normGoldensForUnit "some-size" 'P' (MkSomeSize SP . MkBytes),
        U.normGoldensForUnit "some-size" 'E' (MkSomeSize SE . MkBytes),
        U.normGoldensForUnit "some-size" 'Z' (MkSomeSize SZ . MkBytes),
        U.normGoldensForUnit "some-size" 'Y' (MkSomeSize SY . MkBytes)
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
