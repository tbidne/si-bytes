-- | Property tests for 'SomeNetDir'.
module Props.Data.Bytes.Network.SomeNetDir (props) where

import Control.Monad (join)
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Network.NetBytes.Internal (SomeNetSize (..))
import Data.Bytes.Network.SomeNetDir.Internal (SomeNet (..), SomeNetDir (..))
import Data.Bytes.Size (SSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Hedgehog ((===))
import Hedgehog qualified as H
import Props.Generators.Network qualified as NGens
import Props.MaxRuns (MaxRuns (..))
import Props.Utils qualified as U
import Props.Verify.Algebra qualified as VAlgebra
import Props.Verify.Conversion qualified as VConv
import Test.Tasty (TestTree)
import Test.Tasty qualified as T

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
