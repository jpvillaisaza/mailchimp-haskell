{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.List.Member
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.List.Member
  ( ListMemberApi
  , ListMemberClient (..)
  , ListMemberRequest (..)
  , makeListMemberRequest
  , ListMemberResponse (..)
  , ListMemberId
  , ListMemberStatus (..)
  )
  where

-- aeson
import Data.Aeson
import qualified Data.Aeson as Aeson

-- base
import GHC.Generics

-- generics-sop
import Generics.SOP

-- mailchimp
import Web.MailChimp.Common

-- servant
import Servant.API

-- servant-client
import Servant.Client
import Servant.Client.Generic

-- text
import Data.Text (Text)


type ListMemberId =
  Id


-- |
--
--

type ListMemberApi =
    ReqBody '[JSON] ListMemberRequest
      :> Post '[JSON] ListMemberResponse

  :<|>
    Get '[JSON] [ListMemberResponse]

  :<|>
    Capture "subscriber_hash" ListMemberId
      :> Get '[JSON] ListMemberResponse

  :<|>
    Capture "subscriber_hash" ListMemberId
      :> ReqBody '[JSON] ListMemberRequest
      :> Patch '[JSON] ListMemberResponse

  :<|>
    Capture "subscriber_hash" ListMemberId
      :> ReqBody '[JSON] ListMemberRequest
      :> Put '[JSON] ListMemberResponse

  :<|>
    Capture "subscriber_hash" ListMemberId
      :> Delete '[JSON] String


-- |
--
--

data ListMemberClient =
  ListMemberClient
    { -- |
      --
      -- Add a new list member.

      addListMember
        :: ListMemberRequest
        -> ClientM ListMemberResponse

      -- |
      --
      -- Get information about members in a list.

    , getListMembers
        :: ClientM [ListMemberResponse]

      -- |
      --
      -- Get information about a specific list member.

    , getListMember
        :: ListMemberId
        -> ClientM ListMemberResponse

      -- |
      --
      -- Update a list member.

    , updateListMember
        :: ListMemberId
        -> ListMemberRequest
        -> ClientM ListMemberResponse

      -- |
      --
      -- Add or update a list member.

    , addOrUpdateListMember
        :: ListMemberId
        -> ListMemberRequest
        -> ClientM ListMemberResponse

      -- |
      --
      -- Remove a list member.

    , deleteListMember
        :: ListMemberId
        -> ClientM String

    }
  deriving (GHC.Generics.Generic)


-- |
--
--

instance Generics.SOP.Generic ListMemberClient


-- |
--
--

instance (Client ListMemberApi ~ client) => ClientLike client ListMemberClient


-- |
--
--

data ListMemberRequest =
  ListMemberRequest
    { listMemberEmailAddress :: Text
    , listMemberMergeFields :: [(Text, Text)]
    , listMemberStatus :: ListMemberStatus
    , listMemberExtra :: [(Text, Aeson.Value)]
    }
  deriving (Show)


-- |
--
-- Create a list member request.

makeListMemberRequest
  :: Text -- ^ Email
  -> ListMemberStatus
  -> ListMemberRequest
makeListMemberRequest emailAddress status =
  ListMemberRequest
    { listMemberEmailAddress = emailAddress
    , listMemberMergeFields = mempty
    , listMemberStatus = status
    , listMemberExtra = mempty
    }


-- |
--
--

instance ToJSON ListMemberRequest where
  toJSON ListMemberRequest {..} =
    Aeson.object $
      "email_address" .= listMemberEmailAddress
        : mergeFields
        : "status" .= listMemberStatus
        : listMemberExtra
    where
      mergeFields =
        ( "merge_fields"
        , Aeson.object (fmap (fmap toJSON) listMemberMergeFields)
        )


-- |
--
--

data ListMemberResponse =
  ListMemberResponse
    { listMemberId :: ListMemberId
    }
  deriving (Show)


-- |
--
--

instance FromJSON ListMemberResponse where
  parseJSON =
    Aeson.withObject "" $
      \o ->
        ListMemberResponse
          <$> o .: "id"


-- |
--
--

data ListMemberStatus
  = Cleaned
  | Pending
  | Subscribed
  | Unsubscribed
  deriving (Show)


-- |
--
--

instance ToJSON ListMemberStatus where
  toJSON Cleaned = "cleaned"
  toJSON Pending = "pending"
  toJSON Subscribed = "subscribed"
  toJSON Unsubscribed = "unsubscribed"
