-- | Entry point for running the unit test suite
module Main (main) where

import Data.Proxy (Proxy (..))
import Props.Data.Bytes qualified as PBytes
import Props.Data.Bytes.Class.Conversion qualified as PConv
import Props.Data.Bytes.Network.NetBytes qualified as PNetBytes
import Props.Data.Bytes.Network.SomeNetDir qualified as PSomeNetDir
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
      [ PConv.props,
        PBytes.props,
        PNetBytes.props,
        PSomeNetDir.props
      ]
