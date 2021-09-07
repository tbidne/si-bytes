-- | Property tests for 'Conversion'.
module ByteTypes.Props.Class.Conversion (props) where

import ByteTypes.Class.Conversion qualified as Conv
import ByteTypes.Data.Size (Size (..))
import ByteTypes.Props.Data.Size.Generators qualified as Gens
import ByteTypes.Props.MaxRuns (MaxRuns (..))
import GHC.Real (Ratio (..))
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props = T.testGroup "ByteTypes.Class.Conversion" [convertProps]

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Convert" $
    H.withTests limit $
      H.property $ do
        (s1, s2, n) <- H.forAll genConvertInput
        let expected = n * sizesToMult s1 s2
            result = Conv.convert s1 s2 n
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

intToMult :: Int -> Rational
intToMult n
  | n >= 0 = (1_000 ^ n) :% 1
  | otherwise = 1 :% (1_000 ^ abs n)

sizesToMult :: Size -> Size -> Rational
sizesToMult x y = intToMult $ szToRank x - szToRank y
