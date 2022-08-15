-- | Provides the 'Unwrapper' class.
--
-- @since 0.1
module Data.Bytes.Class.Wrapper
  ( Unwrapper (..),
  )
where

-- | Abstracts "unwrapper" types for generically retrieving a wrapped value.
--
-- @since 0.1
class Unwrapper a where
  -- | @since 0.1
  type Unwrapped a

  -- | Retrieves the underlying value.
  --
  -- ==== __Examples__
  --
  -- >>> import Data.Bytes (Bytes (..), Size (..), Sized (..))
  -- >>> unwrap (MkBytes @G 7)
  -- 7
  --
  -- >>> unwrap (hideSize $ MkBytes @M 400)
  -- 400
  --
  -- >>> import Data.Bytes.Network (Direction (..), NetBytes (..))
  -- >>> unwrap (MkNetBytesP @Up @G 7)
  -- 7
  --
  -- >>> unwrap (hideSize $ MkNetBytesP @Up @G 7)
  -- 7
  --
  -- @since 0.1
  unwrap :: a -> Unwrapped a
