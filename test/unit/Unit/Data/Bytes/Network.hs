-- | Property tests for Network.
module Unit.Data.Bytes.Network (tests) where

import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting qualified as Formatting
import Data.Bytes.Network.Direction (Direction (..), SDirection (..))
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
import Unit.Golden qualified as Golden
import Unit.Props.Generators.Formatting qualified as FGens
import Unit.Props.Generators.Network qualified as NGens
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
      Golden.convGoldens "net-bytes" (MkNetBytesP @Up @B) (MkNetBytesP @Down @Y),
      normalizeProps,
      normalizeGoldens,
      algebraTests
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

normalizeGoldens :: TestTree
normalizeGoldens = T.testGroup "Normalize Goldens" tests'
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
    [ Golden.convGoldens
        "some-net-size"
        (MkSomeNetSize @B @Up SB . MkNetBytesP)
        (MkSomeNetSize @Y @Down SY . MkNetBytesP),
      someNetSizeNormalizeGoldens,
      someNetSizeAlgebraProps
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

someNetSizeNormalizeGoldens :: TestTree
someNetSizeNormalizeGoldens = T.testGroup "Normalize Goldens" tests'
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
    [ Golden.convGoldens
        "some-net-dir"
        (MkSomeNetDir @Up @B SUp . MkNetBytesP)
        (MkSomeNetDir @Down @Y SDown . MkNetBytesP),
      someNetDirNormalizeGoldens,
      someNetDirEqProps
    ]

someNetDirNormalizeGoldens :: TestTree
someNetDirNormalizeGoldens = T.testGroup "Normalize Goldens" tests'
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
    [ Golden.convGoldens
        "some-net"
        (MkSomeNet SUp SK . MkNetBytesP)
        (MkSomeNet SDown SY . MkNetBytesP),
      someNetNormalizeGoldens,
      someNetParsingTests,
      someNetEqProps
    ]

someNetParsingTests :: TestTree
someNetParsingTests =
  T.testGroup
    "Parsing"
    [ VParsing.parsingRoundTrip genBytes genFmt mkFmt
    ]
  where
    mkFmt (sfmt, dfmt) = Formatting.formatSizedDirected baseFmt sfmt dfmt
    baseFmt = Formatting.MkFloatingFormatter (Just 2)
    genBytes = NGens.genFloatingSomeNet @Double
    genFmt = (,) <$> FGens.genSizedFormatter <*> FGens.genDirectedFormatter

someNetNormalizeGoldens :: TestTree
someNetNormalizeGoldens = T.testGroup "Normalize Goldens" tests'
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

someNetEqProps :: TestTree
someNetEqProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "SomeNet Eq laws" "someNetEqProps" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll NGens.genSomeNet
        y <- H.forAll NGens.genSomeNet
        z <- H.forAll NGens.genSomeNet
        VAlgebra.eqLaws x y z
