-- | Utilities for property tests.
module Props.Utils
  ( -- .*. Logical operators
    (==>),
    (<=>),
    -- .*. Verifying laws
    eqLaws,
    ordLaws,
    numLaws,
    -- .*. Miscellaneous
    isNormalized,
    rationalEq,
    reduce,
  )
where

import ByteTypes.Class.Math.Algebra (Group (..))
import ByteTypes.Class.Math.Scalar
  ( Scalar,
    ScalarEq (..),
    ScalarNum (..),
    ScalarOrd (..),
  )
import ByteTypes.Data.Size (ByteSize (..))
import GHC.Real (Ratio (..))
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

infixr 1 ==>

-- | Logical equivalence.
(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = True
_ <=> _ = False

infixr 1 <=>

-- | Verifies 'Eq' laws for 'BytesEq'.
eqLaws :: (Eq a, Show a) => a -> a -> a -> PropertyT IO ()
eqLaws x y z = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z

  -- reflexivity, symmetry, transitivity
  H.assert $ x == x
  H.assert $ x == y <=> y == x
  H.assert $ x == y && y == z ==> x == z

-- | Verifies 'Ord' laws for 'BytesOrd'.
ordLaws :: (Ord a, Show a) => a -> a -> a -> PropertyT IO ()
ordLaws x y z = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  -- reflexivity, transitivity, antisymmetry
  H.assert $ x <= x
  H.assert $ x <= y && y <= z ==> x <= z
  H.assert $ x <= y && y <= x <=> x == y
  -- additional laws
  H.assert $ x <= y <=> y >= x
  H.assert $ x < y <=> x <= y && x /= y
  H.assert $ x < y <=> y > x

-- | Verify 'Num' laws for 'BytesNum'.
numLaws :: (Eq a, ScalarNum a, Show a) => a -> a -> a -> PropertyT IO ()
numLaws x y z = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z

  -- associativity, commutativity, distributivity
  H.annotateShow (x .+. y, (x .+. y) .+. z)
  H.annotateShow (y .+. z, x .+. (y .+. z))
  H.assert $ (x .+. y) .+. z == x .+. (y .+. z)

  H.annotateShow (x .+. y, y .+. x)
  H.assert $ x .+. y == y .+. x

-- | Verifies that the parameter 'BytesOrd' is normalized, taking care
-- to account for special 'B' and 'PB' rules.
isNormalized :: (Group n, Show n, ScalarOrd n, Num (Scalar n)) => ByteSize -> n -> PropertyT IO ()
isNormalized B x = do
  H.footnoteShow x
  H.assert $ gabs x .< 1_000
isNormalized PB x = do
  H.footnoteShow x
  H.assert $ gabs x .>= 1
isNormalized _ x
  | x .= 0 = pure ()
  | otherwise = do
    H.footnoteShow x
    H.assert $ gabs x .>= 1
    H.assert $ gabs x .< 1_000

-- | Checks equality after 'reduce'ing.
rationalEq :: (Integral q, Show q) => Ratio q -> Ratio q -> PropertyT IO ()
rationalEq x y = reduce x === reduce y

-- | Reduces a 'Ratio'.
reduce :: Integral q => Ratio q -> Ratio q
reduce (_ :% 0) = error "Division by zero when reducing rational"
reduce (n :% d) = (n `quot` d') :% (d `quot` d')
  where
    d' = gcd n d
