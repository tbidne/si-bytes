-- | Property tests for 'AnyNetDir'.
module Props.Data.Network.AnyNetDir (props) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as T

-- | 'TestTree' of properties.
props :: TestTree
props =
  T.testGroup
    "Bytes.Data.Network.AnyNetDir"
    []
