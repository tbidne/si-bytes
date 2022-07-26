-- | Provides the 'NetBytes' and 'SomeNetSize' types as an alternative to
-- "Data.Bytes", for when there is a need to distinguish between
-- downloaded and uploaded bytes.
--
-- @since 0.1
module Data.Bytes.Network.NetBytes
  ( -- * Network Bytes
    NetBytes (..),
    unNetBytesP,
    Internal.netToSize,
    Internal.netToDirection,
    hideNetSize,
    Internal.textToNetBytes,

    -- * Unknown Size
    SomeNetSize,
    Internal.unSomeNetSize,
    Internal.someNetSizeToSize,
    Internal.someNetSizeToDirection,
    Internal.textToSomeNetSize,
  )
where

import Data.Bytes.Network.NetBytes.Internal
  ( NetBytes (..),
    SomeNetSize,
    hideNetSize,
    unNetBytesP,
  )
import Data.Bytes.Network.NetBytes.Internal qualified as Internal
