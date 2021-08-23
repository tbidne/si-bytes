-- | Property tests for 'Size'.
module Props.Data.Size (props) where

import ByteTypes.Data.Size (ByteSize (..))
import ByteTypes.Data.Size qualified as S
import Hedgehog (Gen)
import Hedgehog qualified as H
import Props.Data.Size.Generators qualified as Gens
import Props.Utils qualified as Utils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props = T.testGroup "Bytes.Data.Size" [convertProps]

convertProps :: TestTree
convertProps = TH.testProperty "Convert" $
  H.property $ do
    (s1, s2, n) <- H.forAll genConvertInput
    let expected = n * sizesToMult s1 s2
        result = S.convert s1 s2 n
    H.footnote $ "expected: " <> show expected
    H.footnote $ "result: " <> show result
    H.assert $ Utils.epsEq result expected

genConvertInput :: Gen (ByteSize, ByteSize, Double)
genConvertInput =
  (,,) <$> Gens.genByteSize <*> Gens.genByteSize <*> Gens.genD

szToRank :: Num a => ByteSize -> a
szToRank B = 0
szToRank KB = 1
szToRank MB = 2
szToRank GB = 3
szToRank TB = 4
szToRank PB = 5

intToMult :: Floating a => a -> a
intToMult i = 1_000 ** i

sizesToMult :: Floating a => ByteSize -> ByteSize -> a
sizesToMult x y = intToMult $ szToRank x - szToRank y
