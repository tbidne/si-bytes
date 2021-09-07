-- | Exports functions for verifying 'Normalize' properties.
module ByteTypes.Props.Verify.Normalize
  ( isNormalized,
    normalizeLaws,
  )
where

import ByteTypes.Class.Math.Algebra.Group (Group (..), NonZero)
import ByteTypes.Class.Math.Algebra.Module (Module (..))
import ByteTypes.Class.Math.Algebra.VectorSpace (VectorSpace (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Data.Size (Size (..))
import ByteTypes.Utils ((<=>))
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H

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
isNormalized P x = do
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
  NonZero k ->
  PropertyT IO ()
normalizeLaws x y k nz = do
  -- order preserving
  H.assert $ x == y <=> normalize x == normalize y
  H.assert $ x <= y <=> normalize x <= normalize y

  -- respects group structure
  (gid :: n) === normalize (gid :: n)
  ginv x === normalize (ginv x)
  normalize (x .+. y) === normalize x .+. normalize y
  normalize (x .-. y) === normalize x .-. normalize y

  -- respects module structure
  normalize (x .* k) === normalize x .* k

  -- respects vector space structure
  normalize (x .% nz) === normalize x .% nz
