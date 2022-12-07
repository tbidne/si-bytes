-- | Property tests for Network.
module Unit.Data.Bytes.Network (tests) where

import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting qualified as Formatting
import Data.Bytes.Network (Conversion (convert))
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
import Unit.Golden qualified as Golden
import Unit.Props.Generators.Formatting qualified as FGens
import Unit.Props.Generators.Network qualified as NGens
import Unit.Props.Generators.Parsing qualified as PGens
import Unit.Props.Generators.Size qualified as SGens
import Unit.Props.Verify.Algebra qualified as VAlgebra
import Unit.Props.Verify.Conversion qualified as VConv
import Unit.Props.Verify.Normalize qualified as VNormalize
import Unit.Props.Verify.Parsing qualified as VParsing
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
      Golden.convGoldens "net-bytes" (MkNetBytesP @Up @B) (MkNetBytesP @Down @Y)
    ]

normalizeTests :: TestTree
normalizeTests =
  T.testGroup
    "Normalize"
    [ normalizeProps,
      normalizeGoldens
    ]

formattingTests :: TestTree
formattingTests =
  T.testGroup
    "Formatting"
    [ Golden.formatGoldens "Integrals" "net-bytes-int" (MkNetBytesP @Up @T @Int 50) Golden.intSizeDirFormatters,
      Golden.formatGoldens "Floats" "net-bytes-float" (MkNetBytesP @Down @P @Float 120.3648) Golden.floatSizeDirFormatters
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

normalizeGoldens :: TestTree
normalizeGoldens = T.testGroup "Goldens" tests'
  where
    tests' =
      [ Golden.normGoldensForUnit "net-bytes" 'B' (MkNetBytesP @Up @B),
        Golden.normGoldensForUnit "net-bytes" 'K' (MkNetBytesP @Up @K),
        Golden.normGoldensForUnit "net-bytes" 'M' (MkNetBytesP @Up @M),
        Golden.normGoldensForUnit "net-bytes" 'G' (MkNetBytesP @Up @G),
        Golden.normGoldensForUnit "net-bytes" 'T' (MkNetBytesP @Up @T),
        Golden.normGoldensForUnit "net-bytes" 'P' (MkNetBytesP @Down @P),
        Golden.normGoldensForUnit "net-bytes" 'E' (MkNetBytesP @Down @E),
        Golden.normGoldensForUnit "net-bytes" 'Z' (MkNetBytesP @Down @Z),
        Golden.normGoldensForUnit "net-bytes" 'Y' (MkNetBytesP @Down @Y)
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
      Golden.convGoldens
        "some-net-size"
        (MkSomeNetSize @B @Up SB . MkNetBytesP)
        (MkSomeNetSize @Y @Down SY . MkNetBytesP)
    ]

someNetSizeNormalizeTests :: TestTree
someNetSizeNormalizeTests =
  T.testGroup
    "Normalize"
    [ someNetSizeNormalizeProps,
      someNetSizeNormalizeGoldens
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
    [ Golden.formatGoldens "Integrals" "some-net-size-int" (MkSomeNetSize SE $ MkNetBytesP @Up @_ @Int 50) Golden.intSizeDirFormatters,
      Golden.formatGoldens "Floats" "some-net-size-float" (MkSomeNetSize SZ $ MkNetBytesP @Down @_ @Float 120.3648) Golden.floatSizeDirFormatters
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

someNetSizeNormalizeGoldens :: TestTree
someNetSizeNormalizeGoldens = T.testGroup "Goldens" tests'
  where
    tests' =
      [ Golden.normGoldensForUnit "some-net-size" 'B' (MkSomeNetSize @_ @Up SB . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'K' (MkSomeNetSize @_ @Up SK . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'M' (MkSomeNetSize @_ @Up SM . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'G' (MkSomeNetSize @_ @Up SG . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'T' (MkSomeNetSize @_ @Up ST . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'P' (MkSomeNetSize @_ @Down SP . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'E' (MkSomeNetSize @_ @Down SE . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'Z' (MkSomeNetSize @_ @Down SZ . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-size" 'Y' (MkSomeNetSize @_ @Down SY . MkNetBytesP)
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
      Golden.convGoldens
        "some-net-dir"
        (MkSomeNetDir @Up @B SUp . MkNetBytesP)
        (MkSomeNetDir @Down @Y SDown . MkNetBytesP)
    ]

someNetDirNormalizeTests :: TestTree
someNetDirNormalizeTests =
  T.testGroup
    "Normalize"
    [ someNetDirNormalizeProps,
      someNetDirNormalizeGoldens
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
    [ Golden.formatGoldens "Integrals" "some-net-dir-int" (MkSomeNetDir SUp $ MkNetBytesP @_ @Y @Int 50) Golden.intSizeDirFormatters,
      Golden.formatGoldens "Floats" "some-net-dir-float" (MkSomeNetDir SDown $ MkNetBytesP @_ @K @Float 120.3648) Golden.floatSizeDirFormatters
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

someNetDirNormalizeGoldens :: TestTree
someNetDirNormalizeGoldens = T.testGroup "Goldens" tests'
  where
    tests' =
      [ Golden.normGoldensForUnit "some-net-dir" 'B' (MkSomeNetDir @_ @B SUp . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'K' (MkSomeNetDir @_ @K SUp . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'M' (MkSomeNetDir @_ @M SUp . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'G' (MkSomeNetDir @_ @G SUp . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'T' (MkSomeNetDir @_ @T SUp . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'P' (MkSomeNetDir @_ @P SDown . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'E' (MkSomeNetDir @_ @E SDown . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'Z' (MkSomeNetDir @_ @Z SDown . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'Y' (MkSomeNetDir @_ @Y SDown . MkNetBytesP)
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
      Golden.convGoldens
        "some-net"
        (MkSomeNet SUp SK . MkNetBytesP)
        (MkSomeNet SDown SY . MkNetBytesP)
    ]

someNetNormalizeTests :: TestTree
someNetNormalizeTests =
  T.testGroup
    "Normalize"
    [ someNetNormalizeProps,
      someNetNormalizeGoldens
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

someNetNormalizeGoldens :: TestTree
someNetNormalizeGoldens = T.testGroup "Goldens" tests'
  where
    tests' =
      [ Golden.normGoldensForUnit "some-net-dir" 'B' (MkSomeNet SUp SB . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'K' (MkSomeNet SUp SK . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'M' (MkSomeNet SUp SM . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'G' (MkSomeNet SUp SG . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'T' (MkSomeNet SUp ST . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'P' (MkSomeNet SDown SP . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'E' (MkSomeNet SDown SE . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'Z' (MkSomeNet SDown SZ . MkNetBytesP),
        Golden.normGoldensForUnit "some-net-dir" 'Y' (MkSomeNet SDown SY . MkNetBytesP)
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
    [ Golden.formatGoldens "Integrals" "some-net-int" (MkSomeNet SUp SK $ MkNetBytesP @_ @_ @Int 50) Golden.intSizeDirFormatters,
      Golden.formatGoldens "Floats" "some-net-float" (MkSomeNet SDown SY $ MkNetBytesP @_ @_ @Float 120.3648) Golden.floatSizeDirFormatters
    ]
