-- | Provides the 'PrettyPrint' typeclass.
module ByteTypes.Class.PrettyPrint
  ( PrettyPrint (..),
  )
where

-- | Typeclass for pretty printing.
class PrettyPrint a where
  pretty :: a -> String
