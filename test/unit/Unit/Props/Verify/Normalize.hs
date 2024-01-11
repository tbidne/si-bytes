-- | Exports functions for verifying 'Normalize' properties.
module Unit.Props.Verify.Normalize
  ( isNormalized,
    normalizeLaws,
  )
where

import Data.Bytes.Class.Normalize (Normalize (Norm, normalize))
import Data.Bytes.Size (Size (B, Y))
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Numeric.Algebra
  ( AGroup ((.-.)),
    AMonoid (zero),
    ASemigroup ((.+.)),
    MSemiSpace ((.*)),
    MSpace ((.%)),
    VectorSpace,
  )
import Unit.Utils ((<=>))

-- | Verifies that the parameter numberic value is normalized, taking care
-- to account for special 'B' and 'P' rules.
isNormalized ::
  (Num n, Ord n, Show n) =>
  Size ->
  n ->
  PropertyT IO ()
isNormalized B x = do
  H.footnoteShow x
  H.assert $ x < 1_000
isNormalized Y x = do
  H.footnoteShow x
  H.assert $ x >= 1
isNormalized _ x
  | x == 0 = pure ()
  | otherwise = do
      H.footnoteShow x
      H.assert $ x >= 1
      H.assert $ x < 1_000

-- | Verifies that normalize is a homomorphism. In particular, that it
-- is order-preserving and respects the group/module/vector space
-- structure.
normalizeLaws ::
  forall n k.
  ( Norm n ~ n,
    Ord n,
    Normalize n,
    Show n,
    VectorSpace n k
  ) =>
  n ->
  n ->
  k ->
  k ->
  PropertyT IO ()
normalizeLaws x y k nz = do
  -- order preserving
  H.assert $ x == y <=> normalize x == normalize y
  H.assert $ x <= y <=> normalize x <= normalize y

  -- respects group structure
  (zero :: n) === normalize (zero :: n)
  normalize (x .+. y) === normalize x .+. normalize y
  normalize (x .-. y) === normalize x .-. normalize y

  -- respects module structure
  normalize (x .* k) === normalize x .* k

  -- respects vector space structure
  normalize (x .% nz) === normalize x .% nz
