-- | Provides the core alternative to 'ByteTypes.Data.Bytes', for when there
-- is a need to distinguish between downloaded and uploaded bytes.
module ByteTypes.Data.Network.NetBytes
  ( -- * Network Bytes
    NetBytes (MkNetBytesP),
    I.unNetBytesP,
    I.netToSSize,
    I.netToSDirection,

    -- * Unknown Size
    SomeNetSize (..),
    I.someNetSizeToSDirection,
  )
where

import ByteTypes.Data.Network.NetBytes.Internal (NetBytes, SomeNetSize (..))
import ByteTypes.Data.Network.NetBytes.Internal qualified as I
