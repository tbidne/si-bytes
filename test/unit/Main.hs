-- | Entry point for running the unit test suite
module Main (main) where

import Props.Data.Bytes qualified as PBytes
import Props.Data.Size qualified as PSize
import Test.Tasty qualified as T

-- | Runs all property tests
main :: IO ()
main =
  T.defaultMain $
    T.testGroup
      "Property tests"
      [ PSize.props,
        PBytes.props
      ]
