-- | Provides the 'NetBytes' and 'SomeNetSize' types as an alternative to
-- "ByteTypes.Data.Bytes", for when there is a need to distinguish between
-- downloaded and uploaded bytes.
--
-- @since 0.1
module ByteTypes.Data.Network.NetBytes
  ( -- * Network Bytes
    NetBytes (MkNetBytesP),
    unNetBytesP,
    Internal.netToSize,
    Internal.netToDirection,
    hideNetSize,

    -- * Unknown Size
    SomeNetSize,
    Internal.unSomeNetSize,
    Internal.someNetSizeToSize,
    Internal.someNetSizeToDirection,
  )
where

import ByteTypes.Data.Network.NetBytes.Internal
  ( NetBytes (MkNetBytesP),
    SomeNetSize,
    hideNetSize,
    unNetBytesP,
  )
import ByteTypes.Data.Network.NetBytes.Internal qualified as Internal
