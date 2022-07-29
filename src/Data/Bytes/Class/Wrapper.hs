-- | Provides the 'Unwrapper' class.
--
-- @since 0.1
module Data.Bytes.Class.Wrapper
  ( Unwrapper (..),
  )
where

-- | Abstracts "unwrapper" types for generically retrieving a wrapped value.
--
-- >>> import Data.Bytes
-- >>> unwrap (MkBytes @G 7)
-- 7
--
-- >>> unwrap (hideSize $ MkBytes @M 400)
-- 400
--
-- @since 0.1
class Unwrapper a where
  -- | @since 0.1
  type Unwrapped a

  -- | Retrieves the underlying type.
  --
  -- @since 0.1
  unwrap :: a -> Unwrapped a
