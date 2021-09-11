-- | The main entry point to the library. Provides the types and classes for
-- working with different byte sizes (e.g. B, K, M ...). See
-- 'ByteTypes.Data.Network' if there is a need to distinguish between
-- downloaded and uploaded bytes.
module ByteTypes.Data.Bytes
  ( -- * Bytes
    Bytes (..),

    -- * Unknown Size
    SomeSize,
    hideSize,
  )
where

import ByteTypes.Data.Bytes.Internal (Bytes (..), SomeSize, hideSize)
