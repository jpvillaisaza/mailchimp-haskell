{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
  , ListMemberClient(..)
  , ListMemberRequest(..)
  , makeListMemberRequest
  , ListMemberResponse(..)
  , ListMemberId
  , ListMemberStatus(..)
  )
  where

-- aeson
import Data.Aeson
import qualified Data.Aeson as Aeson

-- mailchimp
import Web.MailChimp.Common

-- servant
import Servant.API

-- servant-client
import Servant.Client

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.IO.Class (MonadIO)


type ListMemberId =
  Id

-- |
--
--

type ListMemberApi =
       ReqBody '[JSON] ListMemberRequest
         :> Post '[JSON] ListMemberResponse

  :<|> Get '[JSON] [ListMemberResponse]

  :<|> Capture "subscriber_hash" ListMemberId
         :> Get '[JSON] ListMemberResponse

  :<|> Capture "subscriber_hash" ListMemberId
         :> ReqBody '[JSON] ListMemberRequest
         :> Patch '[JSON] ListMemberResponse

  :<|> Capture "subscriber_hash" ListMemberId
         :> ReqBody '[JSON] ListMemberRequest
         :> Put '[JSON] ListMemberResponse


  :<|> Capture "subscriber_hash" ListMemberId
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
        :: forall m . MonadIO m
        => ListMemberRequest
        -> m (Either ServantError ListMemberResponse)

      -- |
      --
      -- Get information about members in a list.

    , getListMembers
        :: forall m . MonadIO m
        => m (Either ServantError [ListMemberResponse])

      -- |
      --
      -- Get information about a specific list member.

    , getListMember
        :: forall m . MonadIO m
        => ListMemberId
        -> m (Either ServantError ListMemberResponse)

      -- |
      --
      -- Update a list member.

    , updateListMember
        :: forall m . MonadIO m
        => ListMemberId
        -> ListMemberRequest
        -> m (Either ServantError ListMemberResponse)

      -- |
      --
      -- Add or update a list member.

    , addOrUpdateListMember
        :: forall m . MonadIO m
        => ListMemberId
        -> ListMemberRequest
        -> m (Either ServantError ListMemberResponse)

      -- |
      --
      -- Remove a list member.

    , deleteListMember
        :: forall m . MonadIO m
        => ListMemberId
        -> m (Either ServantError String)

    }


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
