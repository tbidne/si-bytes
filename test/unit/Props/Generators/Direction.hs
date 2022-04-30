-- | Exports generators for 'Direction'.
module Props.Generators.Direction
  ( genDirection,
  )
where

import Data.Bytes.Network.Direction (Direction (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen

-- | Uniform distribution over 'Direction'.
genDirection :: Gen Direction
genDirection = HGen.element [Down, Up]
