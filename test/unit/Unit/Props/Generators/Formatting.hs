-- | Exports generators for formatting
module Unit.Props.Generators.Formatting
  ( genSizedFormatter,
    genDirectedFormatter,
  )
where

import Data.Bytes.Formatting.Direction
  ( DirectedFormatter,
    directedFormatterUnix,
    directedFormatterVerbose,
  )
import Data.Bytes.Formatting.Size
  ( SizedFormatter,
    sizedFormatterNatural,
    sizedFormatterUnix,
    sizedFormatterVerbose,
  )
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HGen

-- | Generates a sized formatter
genSizedFormatter :: Gen SizedFormatter
genSizedFormatter =
  HGen.element
    [ sizedFormatterUnix,
      sizedFormatterNatural,
      sizedFormatterVerbose
    ]

-- | Generates a directed formatter
genDirectedFormatter :: Gen DirectedFormatter
genDirectedFormatter =
  HGen.element
    [ directedFormatterUnix,
      directedFormatterVerbose
    ]
