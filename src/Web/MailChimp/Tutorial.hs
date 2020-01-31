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
  where

-- base
import System.Environment (getEnv)

-- bytestring
import Data.ByteString.Char8 (pack)

-- mailchimp
import Web.MailChimp
import Web.MailChimp.Extra
import Web.MailChimp.List
import Web.MailChimp.List.Member

-- text
import qualified Data.Text as Text

example :: IO ()
example = do
  manager <- makeManager
  key <- fmap pack (getEnv "MAILCHIMP_API_KEY")
  listId <- fmap Text.pack (getEnv "MAILCHIMP_LIST_ID")

  let
    basicAuthData = makeBasicAuthData key
  let
    _member =
      (makeListMemberRequest "sd@sd.com" Pending)
        { listMemberMergeFields = [("FNAME", "Juan")]
        }

  -- eitherAdd <- run manager key (addListMember listMemberClient basicAuthData listId member)
  -- eitherAdd <- run manager key (getListMembers listMemberClient basicAuthData listId Nothing)
  eitherAdd <- run manager key (getAllListMembers basicAuthData listId)
  -- eitherAdd <- run manager key (getLinks mainClient basicAuthData)
  -- eitherAdd <- run manager key (getLists listClient basicAuthData)

  case eitherAdd of
    Left err ->
      putStrLn $ "Error: " ++ show err

    Right msg ->
      print msg
