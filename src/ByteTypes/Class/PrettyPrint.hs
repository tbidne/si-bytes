-- | Provides the 'PrettyPrint' typeclass.
module ByteTypes.Class.PrettyPrint
  ( PrettyPrint (..),
  )
where

import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Direction (SByteDirection (..), SingByteDirection (..))
import ByteTypes.Data.Network (AnyNet (..), AnyNetSize (..), NetBytes (..))
import ByteTypes.Data.Size (SByteSize (..), SingByteSize (..))
import Text.Printf (PrintfArg (..))
import Text.Printf qualified as Pf

-- | Typeclass for pretty printing.
class PrettyPrint a where
  pretty :: a -> String

instance (PrintfArg n, SingByteSize s) => PrettyPrint (Bytes s n) where
  pretty (MkBytes x) = case singByteSize @s of
    SB -> Pf.printf "%.2f" x <> " B"
    SKB -> Pf.printf "%.2f" x <> " KB"
    SMB -> Pf.printf "%.2f" x <> " MB"
    SGB -> Pf.printf "%.2f" x <> " GB"
    STB -> Pf.printf "%.2f" x <> " TB"
    SPB -> Pf.printf "%.2f" x <> " PB"

instance PrintfArg n => PrettyPrint (AnySize n) where
  pretty (MkAnySize sz b) = case sz of
    SB -> pretty b
    SKB -> pretty b
    SMB -> pretty b
    SGB -> pretty b
    STB -> pretty b
    SPB -> pretty b

instance forall d s n. (PrintfArg n, SingByteDirection d, SingByteSize s) => PrettyPrint (NetBytes d s n) where
  pretty (MkNetBytes x) = case singByteDirection @d of
    SDown -> pretty x <> " Down"
    SUp -> pretty x <> " Up"

instance (PrintfArg n, SingByteDirection d) => PrettyPrint (AnyNetSize d n) where
  pretty (MkAnyNetSize sz b) = case sz of
    SB -> pretty b
    SKB -> pretty b
    SMB -> pretty b
    SGB -> pretty b
    STB -> pretty b
    SPB -> pretty b

instance PrintfArg n => PrettyPrint (AnyNet n) where
  pretty (MkAnyNet dir x) = case dir of
    SDown -> pretty x
    SUp -> pretty x
