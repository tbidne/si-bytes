-- | Exports generators for 'Direction'.
module ByteTypes.Props.Data.Direction.Generators
  ( genDirection,
  )
where

import ByteTypes.Data.Direction (Direction (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen

-- | Uniform distribution over 'Direction'.
genDirection :: Gen Direction
genDirection = HGen.element [Down, Up]
