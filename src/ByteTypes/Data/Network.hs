-- | These modules provides a slightly more complicated alternative to
-- 'ByteTypes.Data.Bytes', for when there is a need to distinguish between
-- downloaded and uploaded bytes.
--
-- @since 0.1
module ByteTypes.Data.Network
  ( -- * Core types
    module ByteTypes.Data.Network.NetBytes,

    -- * Direction Existentials
    module ByteTypes.Data.Network.SomeNetDir,
  )
where

import ByteTypes.Data.Network.NetBytes
import ByteTypes.Data.Network.SomeNetDir
