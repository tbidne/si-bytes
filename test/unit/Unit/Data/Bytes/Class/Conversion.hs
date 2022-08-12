-- | Property tests for 'Conversion'.
module Unit.Data.Bytes.Class.Conversion (props) where

import Data.Bytes.Class.Conversion qualified as Conv
import Data.Bytes.Size (Size (..))
import GHC.Real (Ratio (..))
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.Generators.Size qualified as Gens
import Unit.Props.MaxRuns (MaxRuns (..))
import Unit.Props.Utils qualified as U

-- | 'TestTree' of properties.
props :: TestTree
props = T.testGroup "Data.Bytes.Class.Conversion" [convertProps]

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "Convert" "convertProps" $
    H.withTests limit $
      H.property $ do
        (s1, s2, n) <- H.forAll genConvertInput
        let expected = n * sizesToMult s1 s2
            result = Conv.convert' s1 s2 n
        H.footnote $ "expected: " <> show expected
        H.footnote $ "result: " <> show result
        result === expected

genConvertInput :: Gen (Size, Size, Rational)
genConvertInput =
  (,,) <$> Gens.genSize <*> Gens.genSize <*> Gens.genD

szToRank :: Size -> Int
szToRank B = 0
szToRank K = 1
szToRank M = 2
szToRank G = 3
szToRank T = 4
szToRank P = 5
szToRank E = 6
szToRank Z = 7
szToRank Y = 8

intToMult :: Int -> Rational
intToMult n
  | n >= 0 = (1_000 ^ n) :% 1
  | otherwise = 1 :% (1_000 ^ abs n)

sizesToMult :: Size -> Size -> Rational
sizesToMult x y = intToMult $ szToRank x - szToRank y
