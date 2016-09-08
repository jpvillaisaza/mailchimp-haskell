{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.Tutorial
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.Tutorial
  where

-- mailchimp
import Web.MailChimp

-- aeson
import Data.Aeson


example :: IO ()
example = do
  manager <- makeManager
  let
    key = ""
    listId = "3b736efca9"
    member =
      (makeListMemberRequest "sd@sd.com" "pending")
        {listMemberMergeFields = [("FNAME", "Juan")]
        , listMemberExtra = [("vip", toJSON True)]}
  let Just Client{..} = makeClient manager key
  let ListClient{..} = makeListClient listId
--  let ListMemberClient{..} = listMemberClient
  let Just ListMemberClient{..} = makeListMemberClient manager key listId
  res <- addListMember member
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> print msg
