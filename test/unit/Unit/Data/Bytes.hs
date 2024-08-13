{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4, 16, 0)
{-# LANGUAGE OverloadedRecordDot #-}
#endif

-- | Property tests for 'Bytes'.
module Unit.Data.Bytes (tests) where

import Data.Bytes qualified as Bytes
import Data.Bytes.Class.Conversion (Conversion (convert_))
import Data.Bytes.Class.Normalize (Normalize (normalize))
import Data.Bytes.Class.RawNumeric (RawNumeric (toRaw))
import Data.Bytes.Formatting qualified as Formatting
import Data.Bytes.Internal (Bytes (MkBytes), SomeSize (MkSomeSize))
import Data.Bytes.Size
  ( SSize (SB, SE, SG, SK, SM, SP, ST, SY, SZ),
    Size (B, E, G, K, M, P, T, Y, Z),
  )
import Data.Bytes.Size qualified as Size
import Data.Proxy (Proxy (Proxy))
import Hedgehog ((===))
import Hedgehog qualified as H
import Optics.Core (review, view)
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.Generators.Bytes qualified as Gens
import Unit.Props.Generators.Formatting qualified as FGens
import Unit.Props.Generators.Parsing qualified as PGens
import Unit.Props.Generators.Size qualified as SGens
import Unit.Props.Verify.Algebra qualified as VAlgebra
import Unit.Props.Verify.Conversion qualified as VConv
import Unit.Props.Verify.Normalize qualified as VNormalize
import Unit.Props.Verify.Parsing qualified as VParsing
import Unit.Specs.Verify.Conversion qualified as VSpecsConv
import Unit.Specs.Verify.Formatting qualified as VSpecsFmt
import Unit.Specs.Verify.Normalize qualified as VSpecsNorm
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
      bytesAccessorsProps,
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
    [ VSpecsFmt.formatSpecs
        "Integrals"
        (MkBytes @B @Int 50)
        VSpecsFmt.intSizedFormatters
        [ "50B",
          "50 b",
          "50 bytes",
          "50 b",
          "50 Bytes"
        ],
      VSpecsFmt.formatSpecs
        "Floats"
        (MkBytes @K @Float 120.3648)
        VSpecsFmt.floatSizedFormatters
        [ "120.36K",
          "120.365 kb",
          "120.3648 kilobytes",
          "120.36480 kb",
          "120.3648 Kilobytes"
        ]
    ]

parsingTests :: TestTree
parsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsesText "Integrals" PGens.genIntBytesText (Proxy @(Bytes B Integer)),
      VParsing.parsesText "Floats" PGens.genFloatBytesText (Proxy @(Bytes B Double))
    ]

unBytesProps :: TestTree
unBytesProps =
  U.testPropertyCompat "Bytes toRaw + wrap is a no-op" "unBytesProps" $
    H.property $ do
      (MkSomeSize _ bytes) <- H.forAll Gens.genSomeBytes
      bytes === MkBytes (toRaw bytes)

{- ORMOLU_DISABLE -}

bytesAccessorsProps :: TestTree
bytesAccessorsProps =
  U.testPropertyCompat "Bytes accessors" "bytesAccessorsProps" $
    H.property $ do
      bytes@(MkBytes x) <- H.forAll Gens.genBytes
#if MIN_VERSION_base(4, 16, 0)
      x === bytes.unBytes
#endif
      x === view #unBytes bytes
      bytes === review #unBytes x
      x === view Bytes._MkBytes bytes
      bytes === review Bytes._MkBytes x

{- ORMOLU_ENABLE -}

convertTests :: TestTree
convertTests =
  T.testGroup
    "Conversions"
    [ convertProps,
      VSpecsConv.convSpecs expected (MkBytes @B) (MkBytes @Y)
    ]
  where
    expected =
      VSpecsConv.MkExpectedConvs
        { VSpecsConv.bExp = 1_000_000_000_000_000_000_000_000,
          VSpecsConv.kExp = 1_000_000_000_000_000_000_000,
          VSpecsConv.mExp = 1_000_000_000_000_000_000,
          VSpecsConv.gExp = 1_000_000_000_000_000,
          VSpecsConv.tExp = 1_000_000_000_000,
          VSpecsConv.pExp = 1_000_000_000,
          VSpecsConv.eExp = 1_000_000,
          VSpecsConv.zExp = 1_000,
          VSpecsConv.yExp = 1
        }

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
      normalizeSpecs
    ]

normalizeProps :: TestTree
normalizeProps =
  U.testPropertyCompat "Value is normalized" "normalizeProps" $
    H.property $ do
      (MkSomeSize sz bytes) <- H.forAll Gens.genSomeBytes
      let normalized@(MkSomeSize _ (MkBytes x)) = Size.withSingSize sz $ normalize bytes
          label = someSizeToLabel normalized
      H.footnote $ "original: " <> show bytes
      H.footnote $ "normalized: " <> show normalized
      VNormalize.isNormalized label x

normalizeSpecs :: TestTree
normalizeSpecs = T.testGroup "Specs" tests'
  where
    tests' =
      [ VSpecsNorm.normSpecs
          'B'
          (MkBytes @B)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SB (MkBytes 0.25)"
              "MkSomeSize SB (MkBytes 750.0)"
              "MkSomeSize SK (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'K'
          (MkBytes @K)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SB (MkBytes 250.0)"
              "MkSomeSize SK (MkBytes 750.0)"
              "MkSomeSize SM (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'M'
          (MkBytes @M)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SK (MkBytes 250.0)"
              "MkSomeSize SM (MkBytes 750.0)"
              "MkSomeSize SG (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'G'
          (MkBytes @G)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SM (MkBytes 250.0)"
              "MkSomeSize SG (MkBytes 750.0)"
              "MkSomeSize ST (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'T'
          (MkBytes @T)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SG (MkBytes 250.0)"
              "MkSomeSize ST (MkBytes 750.0)"
              "MkSomeSize SP (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'P'
          (MkBytes @P)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize ST (MkBytes 250.0)"
              "MkSomeSize SP (MkBytes 750.0)"
              "MkSomeSize SE (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'E'
          (MkBytes @E)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SP (MkBytes 250.0)"
              "MkSomeSize SE (MkBytes 750.0)"
              "MkSomeSize SZ (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'Z'
          (MkBytes @Z)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SE (MkBytes 250.0)"
              "MkSomeSize SZ (MkBytes 750.0)"
              "MkSomeSize SY (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'Y'
          (MkBytes @Y)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SZ (MkBytes 250.0)"
              "MkSomeSize SY (MkBytes 750.0)"
              "MkSomeSize SY (MkBytes 1500.0)"
          )
      ]

bytesEqProps :: TestTree
bytesEqProps =
  U.testPropertyCompat "Bytes Eq laws" "bytesEqProps" $
    H.property $ do
      x <- H.forAll (Gens.genBytes @'P)
      y <- H.forAll (Gens.genBytes @'P)
      z <- H.forAll (Gens.genBytes @'P)
      VAlgebra.eqLaws x y z

bytesOrdProps :: TestTree
bytesOrdProps =
  U.testPropertyCompat "Bytes Ord laws" "bytesOrdProps" $
    H.property $ do
      x <- H.forAll (Gens.genBytes @'P)
      y <- H.forAll (Gens.genBytes @'P)
      z <- H.forAll (Gens.genBytes @'P)
      VAlgebra.ordLaws x y z

bytesGroupProps :: TestTree
bytesGroupProps =
  U.testPropertyCompat "Bytes Group laws" "bytesGroupProps" $
    H.property $ do
      x <- H.forAll (Gens.genBytes @'P)
      y <- H.forAll (Gens.genBytes @'P)
      z <- H.forAll (Gens.genBytes @'P)
      VAlgebra.groupLaws x y z

bytesVectorSpaceProps :: TestTree
bytesVectorSpaceProps =
  U.testPropertyCompat "Bytes Vector Space laws" "bytesVectorSpaceProps" $
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
    [ someAccessorsProps,
      someConvertTests,
      someNormalizeTests,
      someAlgebraTests,
      someFormattingTests,
      someParsingTests
    ]

{- ORMOLU_DISABLE -}

someAccessorsProps :: TestTree
someAccessorsProps =
  U.testPropertyCompat "SomeSize accessors" "someAccessorsProps" $
    H.property $ do
      someSize@(MkSomeSize sz bytes@(MkBytes x)) <- H.forAll Gens.genSomeBytes
      let bytesB = Size.withSingSize sz $ convert_ @_ @B Proxy bytes

#if MIN_VERSION_base(4, 16, 0)
      x === someSize.unSomeSize
#endif
      x === view #unSomeSize someSize
      bytesB === view Bytes._MkSomeSize someSize
      someSize === review Bytes._MkSomeSize bytesB

{- ORMOLU_ENABLE -}

someConvertTests :: TestTree
someConvertTests =
  T.testGroup
    "Conversions"
    [ someConvertProps,
      VSpecsConv.convSpecs expected (MkSomeSize SB . MkBytes @B) (MkSomeSize SY . MkBytes @Y)
    ]
  where
    expected =
      VSpecsConv.MkExpectedConvs
        { VSpecsConv.bExp = 1_000_000_000_000_000_000_000_000,
          VSpecsConv.kExp = 1_000_000_000_000_000_000_000,
          VSpecsConv.mExp = 1_000_000_000_000_000_000,
          VSpecsConv.gExp = 1_000_000_000_000_000,
          VSpecsConv.tExp = 1_000_000_000_000,
          VSpecsConv.pExp = 1_000_000_000,
          VSpecsConv.eExp = 1_000_000,
          VSpecsConv.zExp = 1_000,
          VSpecsConv.yExp = 1
        }

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
      someNormalizeSpecs
    ]

someNormalizeSpecs :: TestTree
someNormalizeSpecs = T.testGroup "Specs" tests'
  where
    tests' =
      [ VSpecsNorm.normSpecs
          'B'
          (MkSomeSize SB . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SB (MkBytes 0.25)"
              "MkSomeSize SB (MkBytes 750.0)"
              "MkSomeSize SK (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'K'
          (MkSomeSize SK . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SB (MkBytes 250.0)"
              "MkSomeSize SK (MkBytes 750.0)"
              "MkSomeSize SM (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'M'
          (MkSomeSize SM . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SK (MkBytes 250.0)"
              "MkSomeSize SM (MkBytes 750.0)"
              "MkSomeSize SG (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'G'
          (MkSomeSize SG . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SM (MkBytes 250.0)"
              "MkSomeSize SG (MkBytes 750.0)"
              "MkSomeSize ST (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'T'
          (MkSomeSize ST . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SG (MkBytes 250.0)"
              "MkSomeSize ST (MkBytes 750.0)"
              "MkSomeSize SP (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'P'
          (MkSomeSize SP . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize ST (MkBytes 250.0)"
              "MkSomeSize SP (MkBytes 750.0)"
              "MkSomeSize SE (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'E'
          (MkSomeSize SE . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SP (MkBytes 250.0)"
              "MkSomeSize SE (MkBytes 750.0)"
              "MkSomeSize SZ (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'Z'
          (MkSomeSize SZ . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SE (MkBytes 250.0)"
              "MkSomeSize SZ (MkBytes 750.0)"
              "MkSomeSize SY (MkBytes 1.5)"
          ),
        VSpecsNorm.normSpecs
          'Y'
          (MkSomeSize SY . MkBytes)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeSize SZ (MkBytes 250.0)"
              "MkSomeSize SY (MkBytes 750.0)"
              "MkSomeSize SY (MkBytes 1500.0)"
          )
      ]

someNormalizeProps :: TestTree
someNormalizeProps =
  U.testPropertyCompat "SomeSize matches underlying Bytes" "someNormalizeProps" $
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
someConvertProps =
  U.testPropertyCompat "SomeSize matches underlying Bytes" "someConvertProps" $
    H.property $ do
      someSize@(MkSomeSize sz bytes) <- H.forAll Gens.genSomeBytes

      U.annEquals (convert_ (Proxy @B) someSize) (Size.withSingSize sz (convert_ (Proxy @B) bytes))
      U.annEquals (convert_ (Proxy @K) someSize) (Size.withSingSize sz (convert_ (Proxy @K) bytes))
      U.annEquals (convert_ (Proxy @M) someSize) (Size.withSingSize sz (convert_ (Proxy @M) bytes))
      U.annEquals (convert_ (Proxy @G) someSize) (Size.withSingSize sz (convert_ (Proxy @G) bytes))
      U.annEquals (convert_ (Proxy @T) someSize) (Size.withSingSize sz (convert_ (Proxy @T) bytes))
      U.annEquals (convert_ (Proxy @P) someSize) (Size.withSingSize sz (convert_ (Proxy @P) bytes))
      U.annEquals (convert_ (Proxy @E) someSize) (Size.withSingSize sz (convert_ (Proxy @E) bytes))
      U.annEquals (convert_ (Proxy @Z) someSize) (Size.withSingSize sz (convert_ (Proxy @Z) bytes))
      U.annEquals (convert_ (Proxy @Y) someSize) (Size.withSingSize sz (convert_ (Proxy @Y) bytes))

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
someSizeEqProps =
  U.testPropertyCompat "SomeSize Eq laws" "someSizeEqProps" $
    H.property $ do
      x <- H.forAll Gens.genSomeBytes
      y <- H.forAll Gens.genSomeBytes
      z <- H.forAll Gens.genSomeBytes
      VAlgebra.eqLaws x y z

someSizeOrdProps :: TestTree
someSizeOrdProps =
  U.testPropertyCompat "SomeSize Ord laws" "someSizeOrdProps" $
    H.property $ do
      x <- H.forAll Gens.genSomeBytes
      y <- H.forAll Gens.genSomeBytes
      z <- H.forAll Gens.genSomeBytes
      VAlgebra.ordLaws x y z

someSizeGroupProps :: TestTree
someSizeGroupProps =
  U.testPropertyCompat "SomeSize Group laws" "someSizeGroupProps" $
    H.property $ do
      x <- H.forAll Gens.genSomeBytes
      y <- H.forAll Gens.genSomeBytes
      z <- H.forAll Gens.genSomeBytes
      VAlgebra.groupLaws x y z

someVectorSpaceProps :: TestTree
someVectorSpaceProps =
  U.testPropertyCompat "SomeSize Vector Space laws" "someVectorSpaceProps" $
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
    [ VSpecsFmt.formatSpecs
        "Integrals"
        (MkSomeSize @_ @Int SM $ MkBytes 50)
        VSpecsFmt.intSizedFormatters
        [ "50M",
          "50 mb",
          "50 megabytes",
          "50 mb",
          "50 Megabytes"
        ],
      VSpecsFmt.formatSpecs
        "Floats"
        (MkSomeSize @_ @Float SG $ MkBytes 120.3648)
        VSpecsFmt.floatSizedFormatters
        [ "120.36G",
          "120.365 gb",
          "120.3648 gigabytes",
          "120.36480 gb",
          "120.3648 Gigabytes"
        ]
    ]
