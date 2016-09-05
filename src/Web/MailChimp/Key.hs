----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.Key
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.Key
  ( DataCenter
  , Key
  , parseDataCenter
  )
  where

-- attoparsec
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (anyChar, char, digit, hexadecimal)

-- base
import Control.Applicative

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString.Char8


-- |
--
--

type DataCenter =
  ByteString


-- |
--
--

type Key =
  ByteString


-- |
--
--

parseDataCenter
  :: Key
  -> Either String DataCenter
parseDataCenter =
  parseOnly dataCenterParser


-- |
--
--

dataCenterParser :: Parser DataCenter
dataCenterParser =
  manyTill anyChar (char '-') *> fmap pack (many1 anyChar)
