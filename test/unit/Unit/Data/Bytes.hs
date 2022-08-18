-- | Property tests for 'Bytes'.
module Unit.Data.Bytes (tests) where

import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting qualified as Formatting
import Data.Bytes.Internal (Bytes (..), SomeSize (..))
import Data.Bytes.Network (Conversion (convert))
import Data.Bytes.Size (SSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Data.Proxy (Proxy (Proxy))
import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Golden qualified as Golden
import Unit.Props.Generators.Bytes qualified as Gens
import Unit.Props.Generators.Formatting qualified as FGens
import Unit.Props.Generators.Parsing qualified as PGens
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

--------------------------------------------------------------------------------
------------------------------------ BYTES -------------------------------------
--------------------------------------------------------------------------------

bytesTests :: TestTree
bytesTests =
  T.testGroup
    "Bytes"
    [ unBytesProps,
      convertTests,
      normalizeTests,
      algebraTests,
      formattingTests,
      parsingTests
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

formattingTests :: TestTree
formattingTests =
  T.testGroup
    "Formatting"
    [ Golden.formatGoldens "Integrals" "bytes-int" (MkBytes @B @Int 50) Golden.intSizedFormatters,
      Golden.formatGoldens "Floats" "bytes-float" (MkBytes @K @Float 120.3648) Golden.floatSizedFormatters
    ]

parsingTests :: TestTree
parsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsesText "Integrals" PGens.genIntBytesText (Proxy @(Bytes B Integer)),
      VParsing.parsesText "Floats" PGens.genFloatBytesText (Proxy @(Bytes B Double))
    ]

unBytesProps :: TestTree
unBytesProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Bytes unwrapping + wrap is a no-op" "unBytesProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize _ bytes) <- H.forAll Gens.genSomeBytes
        bytes === MkBytes (unwrap bytes)

convertTests :: TestTree
convertTests =
  T.testGroup
    "Conversions"
    [ convertProps,
      Golden.convGoldens "bytes" (MkBytes @B) (MkBytes @Y)
    ]

convertProps :: TestTree
convertProps =
  T.testGroup
    "Properties"
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

normalizeTests :: TestTree
normalizeTests =
  T.testGroup
    "Normalizations"
    [ normalizeProps,
      normalizeGoldens
    ]

normalizeProps :: TestTree
normalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Value is normalized" "normalizeProps" $
    H.withTests limit $
      H.property $ do
        (MkSomeSize sz bytes) <- H.forAll Gens.genSomeBytes
        let normalized@(MkSomeSize _ (MkBytes x)) = Size.withSingSize sz $ normalize bytes
            label = someSizeToLabel normalized
        H.footnote $ "original: " <> show bytes
        H.footnote $ "normalized: " <> show normalized
        VNormalize.isNormalized label x

normalizeGoldens :: TestTree
normalizeGoldens = T.testGroup "Goldens" tests'
  where
    tests' =
      [ Golden.normGoldensForUnit "bytes" 'B' (MkBytes @B),
        Golden.normGoldensForUnit "bytes" 'K' (MkBytes @K),
        Golden.normGoldensForUnit "bytes" 'M' (MkBytes @M),
        Golden.normGoldensForUnit "bytes" 'G' (MkBytes @G),
        Golden.normGoldensForUnit "bytes" 'T' (MkBytes @T),
        Golden.normGoldensForUnit "bytes" 'P' (MkBytes @P),
        Golden.normGoldensForUnit "bytes" 'E' (MkBytes @E),
        Golden.normGoldensForUnit "bytes" 'Z' (MkBytes @Z),
        Golden.normGoldensForUnit "bytes" 'Y' (MkBytes @Y)
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

--------------------------------------------------------------------------------
---------------------------------- SOME SIZE -----------------------------------
--------------------------------------------------------------------------------

someSizeTests :: TestTree
someSizeTests =
  T.testGroup
    "SomeSize"
    [ someConvertTests,
      someNormalizeTests,
      someAlgebraTests,
      someFormattingTests,
      someParsingTests
    ]

someConvertTests :: TestTree
someConvertTests =
  T.testGroup
    "Conversions"
    [ someConvertProps,
      Golden.convGoldens "some-size" (MkSomeSize SB . MkBytes @B) (MkSomeSize SY . MkBytes @Y)
    ]

someParsingTests :: TestTree
someParsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsingRoundTrip genBytes genFmt mkFmt,
      VParsing.parsesText "Integrals" PGens.genIntSizedBytesText (Proxy @(SomeSize Integer)),
      VParsing.parsesText "Floats" PGens.genFloatSizedBytesText (Proxy @(SomeSize Double))
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

someNormalizeTests :: TestTree
someNormalizeTests =
  T.testGroup
    "Normalize"
    [ someNormalizeProps,
      someNormalizeGoldens
    ]

someNormalizeGoldens :: TestTree
someNormalizeGoldens = T.testGroup "Goldens" tests'
  where
    tests' =
      [ Golden.normGoldensForUnit "some-size" 'B' (MkSomeSize SB . MkBytes),
        Golden.normGoldensForUnit "some-size" 'K' (MkSomeSize SK . MkBytes),
        Golden.normGoldensForUnit "some-size" 'M' (MkSomeSize SM . MkBytes),
        Golden.normGoldensForUnit "some-size" 'G' (MkSomeSize SG . MkBytes),
        Golden.normGoldensForUnit "some-size" 'T' (MkSomeSize ST . MkBytes),
        Golden.normGoldensForUnit "some-size" 'P' (MkSomeSize SP . MkBytes),
        Golden.normGoldensForUnit "some-size" 'E' (MkSomeSize SE . MkBytes),
        Golden.normGoldensForUnit "some-size" 'Z' (MkSomeSize SZ . MkBytes),
        Golden.normGoldensForUnit "some-size" 'Y' (MkSomeSize SY . MkBytes)
      ]

someNormalizeProps :: TestTree
someNormalizeProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize matches underlying Bytes" "someNormalizeProps" $
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

someConvertProps :: TestTree
someConvertProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeSize matches underlying Bytes" "someConvertProps" $
    H.withTests limit $
      H.property $ do
        someSize@(MkSomeSize sz bytes) <- H.forAll Gens.genSomeBytes

        U.annEquals (convert (Proxy @B) someSize) (Size.withSingSize sz (convert (Proxy @B) bytes))
        U.annEquals (convert (Proxy @K) someSize) (Size.withSingSize sz (convert (Proxy @K) bytes))
        U.annEquals (convert (Proxy @M) someSize) (Size.withSingSize sz (convert (Proxy @M) bytes))
        U.annEquals (convert (Proxy @G) someSize) (Size.withSingSize sz (convert (Proxy @G) bytes))
        U.annEquals (convert (Proxy @T) someSize) (Size.withSingSize sz (convert (Proxy @T) bytes))
        U.annEquals (convert (Proxy @P) someSize) (Size.withSingSize sz (convert (Proxy @P) bytes))
        U.annEquals (convert (Proxy @E) someSize) (Size.withSingSize sz (convert (Proxy @E) bytes))
        U.annEquals (convert (Proxy @Z) someSize) (Size.withSingSize sz (convert (Proxy @Z) bytes))
        U.annEquals (convert (Proxy @Y) someSize) (Size.withSingSize sz (convert (Proxy @Y) bytes))

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

someFormattingTests :: TestTree
someFormattingTests =
  T.testGroup
    "Formatting"
    [ Golden.formatGoldens "Integrals" "some-size-int" (MkSomeSize @_ @Int SM $ MkBytes 50) Golden.intSizedFormatters,
      Golden.formatGoldens "Floats" "some-size-float" (MkSomeSize @_ @Float SG $ MkBytes 120.3648) Golden.floatSizedFormatters
    ]
