-- | Exports functions for verifying algebraic laws
module ByteTypes.Props.Verify.Algebra
  ( -- * Verifying laws
    eqLaws,
    ordLaws,
    groupLaws,
    ringLaws,
    fieldLaws,
    moduleLaws,
    vectorSpaceLaws,
  )
where

import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Group (Group (..))
import ByteTypes.Class.Math.Algebra.Module (Module (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Algebra.VectorSpace (VectorSpace (..))
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import ByteTypes.Props.Utils ((<=>), (==>))

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
