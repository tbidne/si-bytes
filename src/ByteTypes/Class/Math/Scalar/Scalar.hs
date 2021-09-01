-- | Provides the 'Scalar' type family.
module ByteTypes.Class.Math.Scalar.Scalar
  ( Scalar,
  )
where

import Data.Kind (Type)

-- | Intended to map a wrapper type to its underlying scalar, e.g.,
--
-- @
-- data Wrapper a = MkWrapper a
-- type instance Scalar (Wrapper a) = a
-- @
type Scalar :: Type -> Type
type family Scalar a
