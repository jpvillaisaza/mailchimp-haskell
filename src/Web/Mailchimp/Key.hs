----------------------------------------------------------------------
-- |
-- Module: Web.Mailchimp.Key
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Mailchimp.Key
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
-- A Mailchimp data center.

type DataCenter =
  ByteString

-- |
--
-- A Mailchimp API key.

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
