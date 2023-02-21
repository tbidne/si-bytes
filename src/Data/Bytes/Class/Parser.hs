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

-- | Represents a megaparsec parser. Used for parsing byte types from
-- 'Text'.
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
parseDigits :: (Read n) => Parsec Void Text n
parseDigits = do
  MPC.space
  b <- MP.takeWhile1P Nothing (\c -> Ch.isDigit c || c == '.')
  MPC.space
  case TR.readMaybe (T.unpack b) of
    Nothing -> fail $ "Could not read digits: " <> T.unpack b
    Just b' -> pure b'
{-# INLINEABLE parseDigits #-}

-- | Parses various byte types from 'Text'. Parsing is
-- lenient in general. We support:
--
-- * Case-insensitivity.
-- * Optional leading\/internal\/trailing whitespace.
-- * Flexible names.
--
-- ==== __Bytes Examples__
--
-- >>> import Data.Bytes (Bytes, Size (..), SomeSize)
-- >>> parse @(Bytes M Int) "70"
-- Right (MkBytes 70)
--
-- >>> parse @(SomeSize Float) "100.45 kilobytes"
-- Right (MkSomeSize SK (MkBytes 100.45))
--
-- >>> parse @(SomeSize Word) "2300G"
-- Right (MkSomeSize SG (MkBytes 2300))
--
-- >>> parse @(SomeSize Float) "5.5 tb"
-- Right (MkSomeSize ST (MkBytes 5.5))
--
-- ==== __Network Examples__
--
-- >>> import Data.Bytes.Network (Direction (..), NetBytes, SomeNet, SomeNetDir, SomeNetSize)
-- >>> parse @(NetBytes Up M Int) "70"
-- Right (MkNetBytes (MkBytes 70))
--
-- >>> parse @(SomeNetSize Down Float) "100.45 kilobytes"
-- Right (MkSomeNetSize SK (MkNetBytes (MkBytes 100.45)))
--
-- >>> parse @(SomeNetSize Up Word) "2300G"
-- Right (MkSomeNetSize SG (MkNetBytes (MkBytes 2300)))
--
-- >>> parse @(SomeNetDir T Word) "2300 up"
-- Right (MkSomeNetDir SUp (MkNetBytes (MkBytes 2300)))
--
-- >>> parse @(SomeNetDir M Word) "2300D"
-- Right (MkSomeNetDir SDown (MkNetBytes (MkBytes 2300)))
--
-- >>> parse @(SomeNet Float) "5.5 tb Up"
-- Right (MkSomeNet SUp ST (MkNetBytes (MkBytes 5.5)))
--
-- >>> parse @(SomeNet Float) "5.5 megabytes DOWN"
-- Right (MkSomeNet SDown SM (MkNetBytes (MkBytes 5.5)))
--
-- @since 0.1
parse :: (Parser a) => Text -> Either Text a
parse t = case MP.runParser parser "Data.Bytes.Class.Parser.parse" t of
  Left err -> Left . T.pack . MP.errorBundlePretty $ err
  Right v -> Right v
{-# INLINEABLE parse #-}
