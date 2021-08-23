-- | Utilities for property tests.
module Props.Utils
  ( -- * Logical operators
    (==>),
    (<=>),

    -- * Verifying laws
    eqLaws,
    ordLaws,
    numLaws,

    -- * Miscellaneous
    isNormalized,
  )
where

import ByteTypes.Class.Num (BytesEq (..), BytesNum (..), BytesOrd (..), Scalar)
import ByteTypes.Data.Size (ByteSize (..))
import Hedgehog (PropertyT)
import Hedgehog qualified as H

-- | Verifies that the parameter 'BytesOrd' is normalized, taking care
-- to account for special 'B' and 'PB' rules.
isNormalized :: (BytesOrd n, Num (Scalar n)) => ByteSize -> n -> Bool
isNormalized B x = x .< 1_000
isNormalized PB x = x .>= 1
isNormalized _ x
  | x .= 0 = True
  | otherwise = x .>= 1 && x .<= 1_000

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
eqLaws ::
  (BytesEq a, Show a, Show (Scalar a)) =>
  a ->
  a ->
  a ->
  Scalar a ->
  PropertyT IO ()
eqLaws x y z k = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  H.annotateShow k

  -- reflexive, symmetry, transitive
  H.assert $ x .=. x
  H.assert $ x .=. y <=> y .=. x
  H.assert $ x .=. y && y .=. z ==> x .=. z
  -- other laws
  H.assert $ x .=. y <=> not (x ./=. y)
  H.assert $ x .= k <=> k =. x <=> not (x ./= k) <=> not (k /=. x)

-- | Verifies 'Ord' laws for 'BytesOrd'.
ordLaws ::
  (BytesOrd a, Show a, Show (Scalar a)) =>
  a ->
  a ->
  a ->
  Scalar a ->
  PropertyT IO ()
ordLaws x y z k = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  H.annotateShow k
  -- reflexive, transitive, antisymmetry
  H.assert $ x .<=. x
  H.assert $ x .<=. y && y .<=. z ==> x .<=. z
  H.assert $ x .<=. y && y .<=. x <=> x .=. y
  -- additional laws
  H.assert $ x .<=. y <=> y .>=. x
  H.assert $ x .<. y <=> x .<=. y && x ./=. y
  H.assert $ x .<. y <=> y .>. x
  -- laws for scalar comparisons
  H.assert $ x .<= k <=> k >=. x
  H.assert $ x .>= k <=> k <=. x
  H.assert $ x .<= k && x .>= k <=> x .= k
  H.assert $ k >=. x && k <=. x <=> k =. x
  H.assert $ x .< k <=> x .<= k && x ./= k
  H.assert $ x .> k <=> x .>= k && x ./= k
  H.assert $ x .< k <=> k >. x
  H.assert $ x .> k <=> k <=. x && x ./= k

-- | Verify 'Num' laws for 'BytesNum'.
numLaws ::
  (BytesEq a, BytesNum a, Show a, Show (Scalar a)) =>
  a ->
  a ->
  a ->
  Scalar a ->
  Scalar a ->
  PropertyT IO ()
numLaws x y z k l = do
  H.annotateShow x
  H.annotateShow y
  H.annotateShow z
  H.annotateShow k

  -- associative, commutative, distributive
  H.assert $ (x .+. y) .+. z .=. x .+. (y .+. z)
  H.assert $ x .+. y .=. y .+. x
  H.assert $ (x .* k) .* l .=. (x .* k) .* l
  H.assert $ k *. (x .+. y) .=. (k *. x) .+. (k *. y)
  H.assert $ (x .+. y) .* k .=. (x .* k) .+. (y .* k)
