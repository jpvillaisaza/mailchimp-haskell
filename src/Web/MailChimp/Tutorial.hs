----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.Tutorial
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.Tutorial
  (
  )
  where

-- |
--
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- 
-- -- base
-- import System.Environment (getEnv)
--
-- -- bytestring
-- import Data.ByteString.Char8 (pack)
--
-- -- mailchimp
-- import Web.MailChimp
--
-- -- text
-- import qualified Data.Text as Text
--
--
-- example :: IO ()
-- example = do
--   manager <- makeManager
--   key <- fmap pack (getEnv "MAILCHIMP_API_KEY")
--   listId <- fmap Text.pack (getEnv "MAILCHIMP_LIST_ID")
--
--   let
--     Just ListMemberClient {..} =
--       makeListMemberClient manager key listId
--
--   let
--     member =
--       (makeListMemberRequest "sd@sd.com" Pending)
--         { listMemberMergeFields = [("FNAME", "Juan")]
--         }
--
--   eitherAdd <- addListMember member
--
--   case eitherAdd of
--     Left err ->
--       putStrLn $ "Error: " ++ show err
--
--     Right msg ->
--       print msg
