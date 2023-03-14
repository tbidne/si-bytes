-- | Property tests for Network.
module Unit.Data.Bytes.Network (tests) where

import Data.Bytes.Class.Conversion (Conversion (convert))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting qualified as Formatting
import Data.Bytes.Network.Direction (Direction (..), SDirection (..))
import Data.Bytes.Network.Direction qualified as Direction
import Data.Bytes.Network.Internal
  ( NetBytes (..),
    SomeNet (..),
    SomeNetDir (..),
    SomeNetSize (..),
  )
import Data.Bytes.Size (SSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Data.Proxy (Proxy (Proxy))
import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.Generators.Formatting qualified as FGens
import Unit.Props.Generators.Network qualified as NGens
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
    "Bytes.Data.Network"
    [ netBytesProps,
      someNetSizeProps,
      someNetDirProps,
      someNetProps
    ]

--------------------------------------------------------------------------------
---------------------------------- NET BYTES -----------------------------------
--------------------------------------------------------------------------------

netBytesProps :: TestTree
netBytesProps =
  T.testGroup
    "NetBytes"
    [ unNetBytesProps,
      convertTests,
      normalizeTests,
      algebraTests,
      formattingTests,
      parsingTests
    ]

convertTests :: TestTree
convertTests =
  T.testGroup
    "Conversions"
    [ convertProps,
      VSpecsConv.convSpecs expected (MkNetBytesP @Up @B) (MkNetBytesP @Down @Y)
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

normalizeTests :: TestTree
normalizeTests =
  T.testGroup
    "Normalize"
    [ normalizeProps,
      normalizeSpecs
    ]

formattingTests :: TestTree
formattingTests =
  T.testGroup
    "Formatting"
    [ VSpecsFmt.formatSpecs
        "Integrals"
        (MkNetBytesP @Up @T @Int 50)
        VSpecsFmt.intSizeDirFormatters
        [ "50T",
          "50T U",
          "50 tb U",
          "50 terabytes up",
          "50 tb up",
          "50 Terabytes U"
        ],
      VSpecsFmt.formatSpecs
        "Floats"
        (MkNetBytesP @Down @P @Float 120.3648)
        VSpecsFmt.floatSizeDirFormatters
        [ "120.36P",
          "120.365P D",
          "120.3648 pb D",
          "120.36480 petabytes down",
          "120.3648 pb down",
          "120.36 Petabytes D"
        ]
    ]

parsingTests :: TestTree
parsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsesText "Integrals" PGens.genIntBytesText (Proxy @(NetBytes Up M Integer)),
      VParsing.parsesText "Floats" PGens.genFloatBytesText (Proxy @(NetBytes Up M Double))
    ]

algebraTests :: TestTree
algebraTests =
  T.testGroup
    "Algebra"
    [ netBytesEqProps,
      netBytesOrdProps,
      netBytesGroupProps,
      netBytesVectorSpaceProps
    ]

unNetBytesProps :: TestTree
unNetBytesProps =
  U.testPropertyCompat "NetBytes unwrapping + wrap is a no-op" "unNetBytesProps" $
    H.property $ do
      (MkSomeNetSize _ bytes) <- H.forAll NGens.genSomeNetSizeUp
      bytes === MkNetBytesP (unwrap bytes)

convertProps :: TestTree
convertProps =
  T.testGroup
    "Conversions"
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
normalizeProps =
  U.testPropertyCompat "NetBytes normalizes" "normalizeProps" $
    H.property $ do
      (MkSomeNetSize sz bytes) <- H.forAll NGens.genSomeNetSizeUp
      let normalized@(MkSomeNetSize _ (MkNetBytesP x)) =
            Size.withSingSize sz $ normalize bytes
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
          (MkNetBytesP @Up @B)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SB (MkNetBytes (MkBytes 0.25))"
              "MkSomeNetSize SB (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SK (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'K'
          (MkNetBytesP @Up @K)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SB (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SK (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SM (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'M'
          (MkNetBytesP @Up @M)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SK (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SM (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SG (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'G'
          (MkNetBytesP @Up @G)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SM (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SG (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize ST (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'T'
          (MkNetBytesP @Up @T)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SG (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize ST (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SP (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'P'
          (MkNetBytesP @Down @P)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize ST (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SP (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SE (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'E'
          (MkNetBytesP @Down @E)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SP (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SE (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SZ (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Z'
          (MkNetBytesP @Down @Z)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SE (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SZ (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SY (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Y'
          (MkNetBytesP @Down @Y)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SZ (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SY (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SY (MkNetBytes (MkBytes 1500.0))"
          )
      ]

netBytesEqProps :: TestTree
netBytesEqProps =
  U.testPropertyCompat "NetBytes Eq laws" "netBytesEqProps" $
    H.property $ do
      x <- H.forAll (NGens.genNet @'Up @'P)
      y <- H.forAll (NGens.genNet @'Up @'P)
      z <- H.forAll (NGens.genNet @'Up @'P)
      VAlgebra.eqLaws x y z

netBytesOrdProps :: TestTree
netBytesOrdProps =
  U.testPropertyCompat "NetBytes Ord laws" "netBytesOrdProps" $
    H.property $ do
      x <- H.forAll (NGens.genNet @'Up @'P)
      y <- H.forAll (NGens.genNet @'Up @'P)
      z <- H.forAll (NGens.genNet @'Up @'P)
      VAlgebra.ordLaws x y z

netBytesGroupProps :: TestTree
netBytesGroupProps =
  U.testPropertyCompat "NetBytes Group laws" "netBytesGroupProps" $
    H.property $ do
      x <- H.forAll (NGens.genNet @'Up @'P)
      y <- H.forAll (NGens.genNet @'Up @'P)
      z <- H.forAll (NGens.genNet @'Up @'P)
      VAlgebra.groupLaws x y z

netBytesVectorSpaceProps :: TestTree
netBytesVectorSpaceProps =
  U.testPropertyCompat "NetBytes Vector Space laws" "netBytesVectorSpaceProps" $
    H.property $ do
      x <- H.forAll (NGens.genNet @'Up @'P)
      y <- H.forAll (NGens.genNet @'Up @'P)
      k <- H.forAll SGens.genNonZero
      l <- H.forAll SGens.genNonZero
      VAlgebra.vectorSpaceLaws x y k l

--------------------------------------------------------------------------------
-------------------------------- SOME NET SIZE ---------------------------------
--------------------------------------------------------------------------------

someNetSizeProps :: TestTree
someNetSizeProps =
  T.testGroup
    "SomeNetSize"
    [ someNetSizeConvertTests,
      someNetSizeNormalizeTests,
      someNetSizeAlgebraProps,
      someNetSizeFormattingTests,
      someNetSizeParsingTests
    ]

someNetSizeConvertTests :: TestTree
someNetSizeConvertTests =
  T.testGroup
    "Conversions"
    [ someNetSizeConvertProps,
      VSpecsConv.convSpecs
        expected
        (MkSomeNetSize @B @Up SB . MkNetBytesP)
        (MkSomeNetSize @Y @Down SY . MkNetBytesP)
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

someNetSizeNormalizeTests :: TestTree
someNetSizeNormalizeTests =
  T.testGroup
    "Normalize"
    [ someNetSizeNormalizeProps,
      someNetSizeNormalizeSpecs
    ]

someNetSizeAlgebraProps :: TestTree
someNetSizeAlgebraProps =
  T.testGroup
    "Algebra"
    [ someNetSizeEqProps,
      someNetSizeOrdProps,
      someNetSizeGroupProps,
      someNetSizeVectorSpaceProps
    ]

someNetSizeParsingTests :: TestTree
someNetSizeParsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsesText "Integrals" PGens.genIntSizedBytesText (Proxy @(SomeNetSize Up Integer)),
      VParsing.parsesText "Floats" PGens.genFloatSizedBytesText (Proxy @(SomeNetSize Up Double))
    ]

someNetSizeFormattingTests :: TestTree
someNetSizeFormattingTests =
  T.testGroup
    "Formatting"
    [ VSpecsFmt.formatSpecs
        "Integrals"
        (MkSomeNetSize SE $ MkNetBytesP @Up @_ @Int 50)
        VSpecsFmt.intSizeDirFormatters
        [ "50E",
          "50E U",
          "50 eb U",
          "50 exabytes up",
          "50 eb up",
          "50 Exabytes U"
        ],
      VSpecsFmt.formatSpecs
        "Floats"
        (MkSomeNetSize SZ $ MkNetBytesP @Down @_ @Float 120.3648)
        VSpecsFmt.floatSizeDirFormatters
        [ "120.36Z",
          "120.365Z D",
          "120.3648 zb D",
          "120.36480 zettabytes down",
          "120.3648 zb down",
          "120.36 Zettabytes D"
        ]
    ]

someNetSizeConvertProps :: TestTree
someNetSizeConvertProps =
  U.testPropertyCompat "SomeNetSize matches underlying NetBytes" "someNetSizeConvertProps" $
    H.property $ do
      someNetSize@(MkSomeNetSize sz bytes) <- H.forAll NGens.genSomeNetSizeDown

      U.annEquals (convert (Proxy @B) someNetSize) (Size.withSingSize sz (convert (Proxy @B) bytes))
      U.annEquals (convert (Proxy @K) someNetSize) (Size.withSingSize sz (convert (Proxy @K) bytes))
      U.annEquals (convert (Proxy @M) someNetSize) (Size.withSingSize sz (convert (Proxy @M) bytes))
      U.annEquals (convert (Proxy @G) someNetSize) (Size.withSingSize sz (convert (Proxy @G) bytes))
      U.annEquals (convert (Proxy @T) someNetSize) (Size.withSingSize sz (convert (Proxy @T) bytes))
      U.annEquals (convert (Proxy @P) someNetSize) (Size.withSingSize sz (convert (Proxy @P) bytes))
      U.annEquals (convert (Proxy @E) someNetSize) (Size.withSingSize sz (convert (Proxy @E) bytes))
      U.annEquals (convert (Proxy @Z) someNetSize) (Size.withSingSize sz (convert (Proxy @Z) bytes))
      U.annEquals (convert (Proxy @Y) someNetSize) (Size.withSingSize sz (convert (Proxy @Y) bytes))

someNetSizeEqProps :: TestTree
someNetSizeEqProps =
  U.testPropertyCompat "SomeNetSize Eq laws" "someNetSizeEqProps" $
    H.property $ do
      x <- H.forAll NGens.genSomeNetSizeUp
      y <- H.forAll NGens.genSomeNetSizeUp
      z <- H.forAll NGens.genSomeNetSizeUp
      VAlgebra.eqLaws x y z

someNetSizeOrdProps :: TestTree
someNetSizeOrdProps =
  U.testPropertyCompat "SomeNetSize Ord laws" "someNetSizeOrdProps" $
    H.property $ do
      x <- H.forAll NGens.genSomeNetSizeUp
      y <- H.forAll NGens.genSomeNetSizeUp
      z <- H.forAll NGens.genSomeNetSizeUp
      VAlgebra.ordLaws x y z

someNetSizeGroupProps :: TestTree
someNetSizeGroupProps =
  U.testPropertyCompat "SomeNetSize Group laws" "someNetSizeGroupProps" $
    H.property $ do
      x <- H.forAll NGens.genSomeNetSizeUp
      y <- H.forAll NGens.genSomeNetSizeUp
      z <- H.forAll NGens.genSomeNetSizeUp
      VAlgebra.groupLaws x y z

someNetSizeVectorSpaceProps :: TestTree
someNetSizeVectorSpaceProps =
  U.testPropertyCompat "SomeNetSize Vector Space laws" "someNetSizeVectorSpaceProps" $
    H.property $ do
      x <- H.forAll NGens.genSomeNetSizeUp
      y <- H.forAll NGens.genSomeNetSizeUp
      k <- H.forAll SGens.genNonZero
      l <- H.forAll SGens.genNonZero
      VAlgebra.vectorSpaceLaws x y k l

someNetSizeNormalizeSpecs :: TestTree
someNetSizeNormalizeSpecs = T.testGroup "Specs" tests'
  where
    tests' =
      [ VSpecsNorm.normSpecs
          'B'
          (MkSomeNetSize @_ @Up SB . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SB (MkNetBytes (MkBytes 0.25))"
              "MkSomeNetSize SB (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SK (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'K'
          (MkSomeNetSize @_ @Up SK . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SB (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SK (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SM (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'M'
          (MkSomeNetSize @_ @Up SM . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SK (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SM (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SG (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'G'
          (MkSomeNetSize @_ @Up SG . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SM (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SG (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize ST (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'T'
          (MkSomeNetSize @_ @Up ST . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SG (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize ST (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SP (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'P'
          (MkSomeNetSize @_ @Down SP . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize ST (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SP (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SE (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'E'
          (MkSomeNetSize @_ @Down SE . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SP (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SE (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SZ (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Z'
          (MkSomeNetSize @_ @Down SZ . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SE (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SZ (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SY (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Y'
          (MkSomeNetSize @_ @Down SY . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNetSize SZ (MkNetBytes (MkBytes 250.0))"
              "MkSomeNetSize SY (MkNetBytes (MkBytes 750.0))"
              "MkSomeNetSize SY (MkNetBytes (MkBytes 1500.0))"
          )
      ]

someNetSizeNormalizeProps :: TestTree
someNetSizeNormalizeProps =
  U.testPropertyCompat "SomeNetSize normalization" "someNetSizeNormalizeProps" $
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

--------------------------------------------------------------------------------
-------------------------------- SOME NET DIR ----------------------------------
--------------------------------------------------------------------------------

someNetDirProps :: TestTree
someNetDirProps =
  T.testGroup
    "SomeNetDir"
    [ someNetDirConvertTests,
      someNetDirNormalizeTests,
      someNetDirAlgebraProps,
      someNetDirFormattingTests,
      someNetDirParsingTests
    ]

someNetDirConvertTests :: TestTree
someNetDirConvertTests =
  T.testGroup
    "Conversions"
    [ someNetDirConvertProps,
      VSpecsConv.convSpecs
        expected
        (MkSomeNetDir @Up @B SUp . MkNetBytesP)
        (MkSomeNetDir @Down @Y SDown . MkNetBytesP)
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

someNetDirNormalizeTests :: TestTree
someNetDirNormalizeTests =
  T.testGroup
    "Normalize"
    [ someNetDirNormalizeProps,
      someNetDirNormalizeSpecs
    ]

someNetDirAlgebraProps :: TestTree
someNetDirAlgebraProps =
  T.testGroup
    "Algebra"
    [ someNetDirEqProps
    ]

someNetDirFormattingTests :: TestTree
someNetDirFormattingTests =
  T.testGroup
    "Formatting"
    [ VSpecsFmt.formatSpecs
        "Integrals"
        (MkSomeNetDir SUp $ MkNetBytesP @_ @Y @Int 50)
        VSpecsFmt.intSizeDirFormatters
        [ "50Y",
          "50Y U",
          "50 yb U",
          "50 yottabytes up",
          "50 yb up",
          "50 Yottabytes U"
        ],
      VSpecsFmt.formatSpecs
        "Floats"
        (MkSomeNetDir SDown $ MkNetBytesP @_ @K @Float 120.3648)
        VSpecsFmt.floatSizeDirFormatters
        [ "120.36K",
          "120.365K D",
          "120.3648 kb D",
          "120.36480 kilobytes down",
          "120.3648 kb down",
          "120.36 Kilobytes D"
        ]
    ]

someNetDirParsingTests :: TestTree
someNetDirParsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsesText "Integrals" PGens.genIntDirectedBytesText (Proxy @(SomeNetDir T Integer)),
      VParsing.parsesText "Floats" PGens.genFloatDirectedBytesText (Proxy @(SomeNetDir T Double))
    ]

someNetDirConvertProps :: TestTree
someNetDirConvertProps =
  U.testPropertyCompat "SomeNetDir matches underlying NetBytes" "someNetDirConvertProps" $
    H.property $ do
      someNetDir@(MkSomeNetDir d bytes) :: SomeNetDir G Rational <- H.forAll NGens.genSomeNetDirDown

      U.annEquals
        (convert (Proxy @B) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @B) bytes))
      U.annEquals
        (convert (Proxy @K) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @K) bytes))
      U.annEquals
        (convert (Proxy @M) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @M) bytes))
      U.annEquals
        (convert (Proxy @G) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @G) bytes))
      U.annEquals
        (convert (Proxy @T) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @T) bytes))
      U.annEquals
        (convert (Proxy @P) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @P) bytes))
      U.annEquals
        (convert (Proxy @E) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @E) bytes))
      U.annEquals
        (convert (Proxy @Z) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @Z) bytes))
      U.annEquals
        (convert (Proxy @Y) someNetDir)
        (Direction.withSingDirection d $ Direction.hideDirection (convert (Proxy @Y) bytes))

someNetDirNormalizeSpecs :: TestTree
someNetDirNormalizeSpecs = T.testGroup "Specs" tests'
  where
    tests' =
      [ VSpecsNorm.normSpecs
          'B'
          (MkSomeNetDir @_ @B SUp . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SB (MkNetBytes (MkBytes 0.25))"
              "MkSomeNet SUp SB (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SK (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'K'
          (MkSomeNetDir @_ @K SUp . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SB (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp SK (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SM (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'M'
          (MkSomeNetDir @_ @M SUp . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SK (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp SM (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SG (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'G'
          (MkSomeNetDir @_ @G SUp . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SM (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp SG (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp ST (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'T'
          (MkSomeNetDir @_ @T SUp . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SG (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp ST (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SP (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'P'
          (MkSomeNetDir @_ @P SDown . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown ST (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SP (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SE (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'E'
          (MkSomeNetDir @_ @E SDown . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown SP (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SE (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SZ (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Z'
          (MkSomeNetDir @_ @Z SDown . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown SE (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SZ (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SY (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Y'
          (MkSomeNetDir @_ @Y SDown . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown SZ (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SY (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SY (MkNetBytes (MkBytes 1500.0))"
          )
      ]

someNetDirNormalizeProps :: TestTree
someNetDirNormalizeProps =
  U.testPropertyCompat "SomeNetDir matches underlying NetBytes" "someNetSizeNormalizeProps" $
    H.property $ do
      x@(MkSomeNetDir d bytes) :: SomeNetDir G Rational <- H.forAll NGens.genSomeNetDirUp
      let normalizedBytes =
            normalize bytes
          hidden =
            Direction.withSingDirection
              d
              (Direction.hideDirection (Size.hideSize normalizedBytes))
      -- matches underlying bytes
      normalize x === hidden

someNetDirEqProps :: TestTree
someNetDirEqProps =
  U.testPropertyCompat "SomeNetDir Eq laws" "someNetDirEqProps" $
    H.property $ do
      x <- H.forAll $ NGens.genSomeNetDirDown @'P
      y <- H.forAll $ NGens.genSomeNetDirDown @'P
      z <- H.forAll $ NGens.genSomeNetDirDown @'P
      VAlgebra.eqLaws x y z

--------------------------------------------------------------------------------
----------------------------------- SOME NET -----------------------------------
--------------------------------------------------------------------------------

someNetProps :: TestTree
someNetProps =
  T.testGroup
    "SomeNet"
    [ someNetConvertTests,
      someNetNormalizeTests,
      someNetAlgebraProps,
      someNetFormattingTests,
      someNetParsingTests
    ]

someNetConvertTests :: TestTree
someNetConvertTests =
  T.testGroup
    "Conversions"
    [ someNetConvertProps,
      VSpecsConv.convSpecs
        expected
        (MkSomeNet SUp SB . MkNetBytesP)
        (MkSomeNet SDown SY . MkNetBytesP)
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

someNetNormalizeTests :: TestTree
someNetNormalizeTests =
  T.testGroup
    "Normalize"
    [ someNetNormalizeProps,
      someNetNormalizeSpecs
    ]

someNetAlgebraProps :: TestTree
someNetAlgebraProps =
  T.testGroup
    "Algebra"
    [ someNetEqProps
    ]

someNetParsingTests :: TestTree
someNetParsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsingRoundTrip genBytes genFmt mkFmt,
      VParsing.parsesText "Integrals" PGens.genIntSizedDirectedBytesText (Proxy @(SomeNet Integer)),
      VParsing.parsesText "Floats" PGens.genFloatSizedDirectedBytesText (Proxy @(SomeNet Double))
    ]
  where
    mkFmt (sfmt, dfmt) = Formatting.formatSizedDirected baseFmt sfmt dfmt
    baseFmt = Formatting.MkFloatingFormatter (Just 2)
    genBytes = NGens.genFloatingSomeNet @Double
    genFmt = (,) <$> FGens.genSizedFormatter <*> FGens.genDirectedFormatter

someNetConvertProps :: TestTree
someNetConvertProps =
  U.testPropertyCompat "SomeNet matches underlying NetBytes" "someNetDirConvertProps" $
    H.property $ do
      someNet@(MkSomeNet d sz bytes) <- H.forAll NGens.genSomeNet

      U.annEquals
        (convert (Proxy @B) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @B) bytes
        )
      U.annEquals
        (convert (Proxy @K) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @K) bytes
        )
      U.annEquals
        (convert (Proxy @M) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @M) bytes
        )
      U.annEquals
        (convert (Proxy @G) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @G) bytes
        )
      U.annEquals
        (convert (Proxy @T) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @T) bytes
        )
      U.annEquals
        (convert (Proxy @P) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @P) bytes
        )
      U.annEquals
        (convert (Proxy @E) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @E) bytes
        )
      U.annEquals
        (convert (Proxy @Z) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @Z) bytes
        )
      U.annEquals
        (convert (Proxy @Y) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @Y) bytes
        )
      U.annEquals
        (convert (Proxy @B) someNet)
        ( Direction.withSingDirection d $
            Direction.hideDirection $
              Size.withSingSize sz $
                convert (Proxy @B) bytes
        )

someNetNormalizeSpecs :: TestTree
someNetNormalizeSpecs = T.testGroup "Specs" tests'
  where
    tests' =
      [ VSpecsNorm.normSpecs
          'B'
          (MkSomeNet SUp SB . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SB (MkNetBytes (MkBytes 0.25))"
              "MkSomeNet SUp SB (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SK (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'K'
          (MkSomeNet SUp SK . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SB (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp SK (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SM (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'M'
          (MkSomeNet SUp SM . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SK (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp SM (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SG (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'G'
          (MkSomeNet SUp SG . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SM (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp SG (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp ST (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'T'
          (MkSomeNet SUp ST . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SUp SG (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SUp ST (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SUp SP (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'P'
          (MkSomeNet SDown SP . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown ST (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SP (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SE (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'E'
          (MkSomeNet SDown SE . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown SP (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SE (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SZ (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Z'
          (MkSomeNet SDown SZ . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown SE (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SZ (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SY (MkNetBytes (MkBytes 1.5))"
          ),
        VSpecsNorm.normSpecs
          'Y'
          (MkSomeNet SDown SY . MkNetBytesP)
          ( VSpecsNorm.MkExpectedNorms
              "MkSomeNet SDown SZ (MkNetBytes (MkBytes 250.0))"
              "MkSomeNet SDown SY (MkNetBytes (MkBytes 750.0))"
              "MkSomeNet SDown SY (MkNetBytes (MkBytes 1500.0))"
          )
      ]

someNetNormalizeProps :: TestTree
someNetNormalizeProps =
  U.testPropertyCompat "SomeNet matches underlying NetBytes" "someNetSizeNormalizeProps" $
    H.property $ do
      x@(MkSomeNet d sz bytes) :: SomeNet Rational <- H.forAll NGens.genSomeNet
      let normalizedBytes = Size.withSingSize sz $ normalize bytes
          hidden =
            Direction.withSingDirection
              d
              (Direction.hideDirection (Size.hideSize normalizedBytes))
      -- matches underlying bytes
      normalize x === hidden

someNetEqProps :: TestTree
someNetEqProps =
  U.testPropertyCompat "SomeNet Eq laws" "someNetEqProps" $
    H.property $ do
      x <- H.forAll NGens.genSomeNet
      y <- H.forAll NGens.genSomeNet
      z <- H.forAll NGens.genSomeNet
      VAlgebra.eqLaws x y z

someNetFormattingTests :: TestTree
someNetFormattingTests =
  T.testGroup
    "Formatting"
    [ VSpecsFmt.formatSpecs
        "Integrals"
        (MkSomeNet SUp SK $ MkNetBytesP @_ @_ @Int 50)
        VSpecsFmt.intSizeDirFormatters
        [ "50K",
          "50K U",
          "50 kb U",
          "50 kilobytes up",
          "50 kb up",
          "50 Kilobytes U"
        ],
      VSpecsFmt.formatSpecs
        "Floats"
        (MkSomeNet SDown SY $ MkNetBytesP @_ @_ @Float 120.3648)
        VSpecsFmt.floatSizeDirFormatters
        [ "120.36Y",
          "120.365Y D",
          "120.3648 yb D",
          "120.36480 yottabytes down",
          "120.3648 yb down",
          "120.36 Yottabytes D"
        ]
    ]
