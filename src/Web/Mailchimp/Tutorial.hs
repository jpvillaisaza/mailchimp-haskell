{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Mailchimp.Tutorial
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Mailchimp.Tutorial
  ( example
  )
  where

-- base
import System.Environment (getEnv)

-- bytestring
import Data.ByteString.Char8 (pack)

-- mailchimp
import Web.Mailchimp

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
    member =
      (makeListMemberRequest "sd@sd.com" Pending)
        { listMemberMergeFields = [("FNAME", "Juan")]
        }

  eitherAdd <- run manager key (addListMember listMemberClient basicAuthData listId member)
  -- eitherAdd <- run manager key (getLinks mainClient basicAuthData)

  case eitherAdd of
    Left err ->
      putStrLn $ "Error: " ++ show err

    Right msg ->
      print msg
