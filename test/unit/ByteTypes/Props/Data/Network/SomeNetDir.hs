-- | Property tests for 'SomeNetDir'.
module ByteTypes.Props.Data.Network.SomeNetDir (props) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Network.SomeNetDir"
    []
