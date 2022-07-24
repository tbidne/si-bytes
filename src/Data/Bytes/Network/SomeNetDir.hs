-- | This module provides types for hiding the
-- 'Data.Bytes.Direction' on
-- 'Data.Bytes.Network.NetBytes'.
--
-- This paradigm differs from the previous size hiding of
-- 'Data.Bytes.Bytes.SomeSize' and
-- 'Data.Bytes.Network.SomeNetSize' in that because we had
-- sensible ways to convert between sizes (e.g. K -> G), we could
-- combine arbitrary byte types by first converting to a common type.
--
-- Here, there is no sensible way to convert between uploaded and downloaded
-- byte directions by design. These units are meant to be kept separate.
-- While the witnesses allows us to recover the types at will (and we can
-- \"forget\" the direction tag by dropping to 'Data.Bytes'),
-- we are much more limited in what we can do. For example, we lose instances
-- like 'Applicative', "Numeric.Algebra".
--
-- @since 0.1
module Data.Bytes.Network.SomeNetDir
  ( SomeNetDir,
    Internal.unSomeNetDir,
    Internal.someNetDirToSize,
    Internal.someNetDirToDirection,
    hideNetDir,
    SomeNet,
    Internal.unSomeNet,
    Internal.someNetToSize,
    Internal.someNetToDirection,
    hideNetSizeDir,
  )
where

import Data.Bytes.Network.SomeNetDir.Internal
  ( SomeNet,
    SomeNetDir,
    hideNetDir,
    hideNetSizeDir,
  )
import Data.Bytes.Network.SomeNetDir.Internal qualified as Internal
