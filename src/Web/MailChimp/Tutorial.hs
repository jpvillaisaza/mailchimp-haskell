{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.Tutorial
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.Tutorial
  ( example
  )
  where

-- base
import System.Environment (getEnv)

-- bytestring
import Data.ByteString.Char8 (pack)

-- mailchimp
import Web.MailChimp

-- text
import qualified Data.Text as Text


example :: IO ()
example = do
  manager <- makeManager
  key <- fmap pack (getEnv "MAILCHIMP_API_KEY")
  listId <- fmap Text.pack (getEnv "MAILCHIMP_LIST_ID")

  let
    AuthClient {..} = makeAuthClientWithKey key
    ListMemberClient {..} = makeListMemberClient listId

  let
    member =
      (makeListMemberRequest "sd@sd.com" Pending)
        { listMemberMergeFields = [("FNAME", "Juan")]
        }

  eitherAdd <- run manager key (addListMember member)
--  eitherAdd <- run manager key (getLinks (BasicAuthData "" key))

  case eitherAdd of
    Left err ->
      putStrLn $ "Error: " ++ show err

    Right msg ->
      print msg
