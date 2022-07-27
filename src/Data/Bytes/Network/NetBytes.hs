-- | Provides the 'NetBytes' and 'SomeNetSize' types as an alternative to
-- "Data.Bytes", for when there is a need to distinguish between
-- downloaded and uploaded bytes.
--
-- @since 0.1
module Data.Bytes.Network.NetBytes
  ( -- * Network Bytes
    NetBytes (..),
    hideNetSize,

    -- * Unknown Size
    SomeNetSize,
  )
where

import Data.Bytes.Network.NetBytes.Internal
  ( NetBytes (..),
    SomeNetSize,
    hideNetSize,
  )
