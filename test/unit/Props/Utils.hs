-- | Utilities for property tests.
module Props.Utils
  ( -- * Logical operators
    (==>),
    (<=>),

    -- * Verifying laws
    eqLaws,
    ordLaws,
    groupLaws,
    ringLaws,
    fieldLaws,
    moduleLaws,
    vectorSpaceLaws,

    -- * Miscellaneous
    isNormalized,
    rationalEq,
    reduce,
  )
where

import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Group (Group (..))
import ByteTypes.Class.Math.Algebra.Module (Module (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Algebra.VectorSpace (VectorSpace (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Class.Math.Scalar.Ord (ScalarEq (..), ScalarOrd (..))
import ByteTypes.Class.Math.Scalar.Scalar (Scalar)
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

-- | Verify 'Group' laws.
groupLaws :: (Eq a, Group a, Show a) => a -> a -> a -> PropertyT IO ()
groupLaws x y z = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  -- identity
  x === x .+. gid
  x === gid .+. x
  -- associativity
  H.annotateShow (x .+. y, (x .+. y) .+. z)
  H.annotateShow (y .+. z, x .+. (y .+. z))
  (x .+. y) .+. z === x .+. (y .+. z)
  -- inverses
  gid === x .+. ginv x
  gid === ginv x .+. x

-- | Verify 'Ring' laws.
ringLaws :: (Eq a, Ring a, Show a) => a -> a -> a -> PropertyT IO ()
ringLaws x y z = do
  groupLaws x y z
  -- group commutativity
  x .+. y === y .+. x
  -- associativity
  (x .*. y) .*. z === x .*. (y .*. z)
  -- identity
  x === x .*. rid
  x === rid .*. x
  -- distributivity
  x .*. (y .+. z) === (x .*. y) .+. (x .*. z)
  (y .+. z) .*. x === (y .*. x) .+. (z .*. x)

-- | Verify 'Field' laws.
fieldLaws :: (Eq a, Field a, Show a) => a -> a -> a -> PropertyT IO ()
fieldLaws x y z = do
  ringLaws x y z
  -- identity
  rid === x .*. finv x
  rid === finv x .*. x

-- | Verify 'Module' laws.
moduleLaws :: forall m r. (Eq m, Module m r, Show m) => m -> m -> r -> r -> PropertyT IO ()
moduleLaws x y k l = do
  -- left-distributivity
  k *. (x .+. y) === (k *. x) .+. (k *. y)
  (k .+. l) *. x === (k *. x) .+. (l *. x)

  -- right-distributivity
  (x .+. y) .* k === (x .* k) .+. (y .* k)
  x .* (k .+. l) === (x .* k) .+. (x .* l)

  (k .*. l) *. x === l *. (k *. x)

  -- identity
  x === rid @r *. x
  x === x .* rid @r

-- | Verify 'VectorSpace' laws.
vectorSpaceLaws :: forall v k. (Eq v, VectorSpace v k, Show v) => v -> v -> k -> k -> PropertyT IO ()
vectorSpaceLaws x y k l = do
  moduleLaws x y k l
  moduleLaws x y (finv k) (finv l)

-- | Verifies that the parameter 'BytesOrd' is normalized, taking care
-- to account for special 'B' and 'PB' rules.
isNormalized :: (Group n, Show n, ScalarOrd n, NumLiteral (Scalar n)) => ByteSize -> n -> PropertyT IO ()
isNormalized B x = do
  H.footnoteShow x
  H.assert $ gabs x .< fromLit 1_000
isNormalized PB x = do
  H.footnoteShow x
  H.assert $ gabs x .>= fromLit 1
isNormalized _ x
  | x .= fromLit 0 = pure ()
  | otherwise = do
    H.footnoteShow x
    H.assert $ gabs x .>= fromLit 1
    H.assert $ gabs x .< fromLit 1_000

-- | Checks equality after 'reduce'ing.
rationalEq :: (Integral q, Show q) => Ratio q -> Ratio q -> PropertyT IO ()
rationalEq x y = reduce x === reduce y

-- | Reduces a 'Ratio'.
reduce :: Integral q => Ratio q -> Ratio q
reduce (_ :% 0) = error "Division by zero when reducing rational"
reduce (n :% d) = (n `quot` d') :% (d `quot` d')
  where
    d' = gcd n d
