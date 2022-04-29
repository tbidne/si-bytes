-- | Provides the core 'Bytes' and 'SomeSize' types for working with different
-- byte sizes (e.g. B, K, M ...). See 'ByteTypes.Data.Network' if there is a
-- need to distinguish between downloaded and uploaded bytes.
--
-- @since 0.1
module ByteTypes.Data.Bytes
  ( -- * Bytes
    Bytes (..),
    Internal.bytesToSize,
    Internal.hideSize,

    -- * Unknown Size
    SomeSize,
    Internal.unSomeSize,
    Internal.someSizeToSize,
  )
where

import ByteTypes.Data.Bytes.Internal (Bytes (..), SomeSize)
import ByteTypes.Data.Bytes.Internal qualified as Internal
