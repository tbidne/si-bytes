-- | These modules provides a slightly more complicated alternative to
-- 'ByteTypes.Data.Bytes', for when there is a need to distinguish between
-- downloaded and uploaded bytes.
module ByteTypes.Data.Network
  ( -- * Core types
    module ByteTypes.Data.Network.NetBytes,

    -- * Direction Existentials
    module ByteTypes.Data.Network.AnyNetDir,
  )
where

import ByteTypes.Data.Network.AnyNetDir
import ByteTypes.Data.Network.NetBytes
