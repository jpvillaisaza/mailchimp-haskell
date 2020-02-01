----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.Key
-- Description:
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
import Data.Attoparsec.ByteString.Char8 (anyChar, char)

-- bytestring
import Data.ByteString.Char8

-- |
--
-- A MailChimp data center.

type DataCenter =
  ByteString

-- |
--
-- A MailChimp API key.

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
