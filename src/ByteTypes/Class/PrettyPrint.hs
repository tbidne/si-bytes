-- | Provides the 'PrettyPrint' typeclass.
module ByteTypes.Class.PrettyPrint
  ( PrettyPrint (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Text.Printf qualified as Pf

-- | Typeclass for pretty printing.
class PrettyPrint a where
  pretty :: a -> String

instance PrettyPrint Double where pretty = Pf.printf "%.2f"

instance PrettyPrint Float where pretty = Pf.printf "%.2f"

instance PrettyPrint Int where pretty = show

instance PrettyPrint Int8 where pretty = show

instance PrettyPrint Int16 where pretty = show

instance PrettyPrint Int32 where pretty = show

instance PrettyPrint Int64 where pretty = show

instance PrettyPrint Integer where pretty = show

instance PrettyPrint Natural where pretty = show

instance PrettyPrint Word where pretty = show

instance PrettyPrint Word8 where pretty = show

instance PrettyPrint Word16 where pretty = show

instance PrettyPrint Word32 where pretty = show

instance PrettyPrint Word64 where pretty = show

instance Show a => PrettyPrint (Ratio a) where pretty = show
