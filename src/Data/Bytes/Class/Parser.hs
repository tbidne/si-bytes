-- | Provides parsing utilities.
--
-- @since 0.1
module Data.Bytes.Class.Parser
  ( Parser (..),
    parseDigits,
    parse,
  )
where

import Data.Char qualified as Ch
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- | Represents a megaparsec parser.
--
-- @since 0.1
class Parser a where
  -- | Megaparsec parser for the given type.
  --
  -- @since 0.1
  parser :: Parsec Void Text a

-- | Parser combinator for digits with a 'Read' instance.
--
-- @since 0.1
parseDigits :: Read n => Parsec Void Text n
parseDigits = do
  MPC.space
  b <- MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c == '.')
  MPC.space
  case TR.readMaybe (T.unpack b) of
    Nothing -> fail $ "Could not read digits: " <> T.unpack b
    Just b' -> pure b'
{-# INLINEABLE parseDigits #-}

-- | Runs a 'Parser'.
--
-- @since 0.1
parse :: Parser a => Text -> Either Text a
parse t = case MP.runParser parser "Data.Bytes.Class.Parser.parse" t of
  Left err -> Left . T.pack . MP.errorBundlePretty $ err
  Right v -> Right v
{-# INLINEABLE parse #-}
