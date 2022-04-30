-- | Provides the 'PrettyPrint' typeclass.
--
-- @since 0.1
module Data.Bytes.Class.PrettyPrint
  ( PrettyPrint (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Text.Printf qualified as Pf

-- | Typeclass for pretty printing.
--
-- @since 0.1
class PrettyPrint a where
  -- | @since 0.1
  pretty :: a -> String
  default pretty :: Show a => a -> String
  pretty = show

-- | @since 0.1
instance PrettyPrint Double where pretty = Pf.printf "%.2f"

-- | @since 0.1
instance PrettyPrint Float where pretty = Pf.printf "%.2f"

-- | @since 0.1
instance PrettyPrint Int

-- | @since 0.1
instance PrettyPrint Int8

-- | @since 0.1
instance PrettyPrint Int16

-- | @since 0.1
instance PrettyPrint Int32

-- | @since 0.1
instance PrettyPrint Int64

-- | @since 0.1
instance PrettyPrint Integer

-- | @since 0.1
instance PrettyPrint Natural

-- | @since 0.1
instance PrettyPrint Word

-- | @since 0.1
instance PrettyPrint Word8

-- | @since 0.1
instance PrettyPrint Word16

-- | @since 0.1
instance PrettyPrint Word32

-- | @since 0.1
instance PrettyPrint Word64

-- | @since 0.1
instance Show a => PrettyPrint (Ratio a)
