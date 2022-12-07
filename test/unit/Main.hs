-- | Entry point for running the unit test suite
module Main (main) where

import Test.Tasty qualified as T
import Unit.Data.Bytes qualified as PBytes
import Unit.Data.Bytes.Class.Conversion qualified as PConv
import Unit.Data.Bytes.Network qualified as PNetwork

-- | Runs all unit tests
main :: IO ()
main =
  T.defaultMain $
    T.testGroup
      "Unit tests"
      [ PConv.tests,
        PBytes.tests,
        PNetwork.tests
      ]
