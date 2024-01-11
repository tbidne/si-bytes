-- | Provides the 'RawNumeric' class.
--
-- @since 0.1
module Data.Bytes.Class.RawNumeric
  ( RawNumeric (..),
  )
where

-- | Abstracts "wrapper" types for generically retrieving a raw numeric value.
--
-- @since 0.1
class RawNumeric a where
  -- | @since 0.1
  type Raw a

  -- | Retrieves the underlying value.
  --
  -- ==== __Examples__
  --
  -- >>> import Data.Bytes (Bytes (..), Size (..), Sized (..))
  -- >>> toRaw (MkBytes @G 7)
  -- 7
  --
  -- >>> toRaw (hideSize $ MkBytes @M 400)
  -- 400
  --
  -- >>> import Data.Bytes.Network (Direction (..), NetBytes (..))
  -- >>> toRaw (MkNetBytesP @Up @G 7)
  -- 7
  --
  -- >>> toRaw (hideSize $ MkNetBytesP @Up @G 7)
  -- 7
  --
  -- @since 0.1
  toRaw :: a -> Raw a
