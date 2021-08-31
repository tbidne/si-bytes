-- | Entry point for running the unit test suite
module Main (main) where

import Data.Proxy (Proxy (..))
import Props.Data.Bytes qualified as PBytes
import Props.Data.Network qualified as PNet
import Props.Data.Size qualified as PSize
import Props.MaxRuns (MaxRuns)
import Test.Tasty qualified as T
import Test.Tasty.Options (OptionDescription (..))

-- | Runs all property tests
main :: IO ()
main = do
  let options = T.includingOptions [Option @MaxRuns Proxy]
      ingredients = options : T.defaultIngredients
  T.defaultMainWithIngredients ingredients $
    T.testGroup
      "Property tests"
      [ PSize.props,
        PBytes.props,
        PNet.props
      ]
