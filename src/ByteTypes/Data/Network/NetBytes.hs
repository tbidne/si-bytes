-- | Provides the core alternative to 'ByteTypes.Data.Bytes', for when there
-- is a need to distinguish between downloaded and uploaded bytes.
module ByteTypes.Data.Network.NetBytes
  ( -- * Network Bytes
    NetBytes (MkNetBytesP),
    unNetBytesP,

    -- * Unknown Size
    SomeNetSize,
    hideNetSize,
  )
where

import ByteTypes.Data.Network.NetBytes.Internal
  ( NetBytes (MkNetBytesP),
    SomeNetSize,
    hideNetSize,
    unNetBytesP,
  )
