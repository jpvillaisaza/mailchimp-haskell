{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Mailchimp.List.Member
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Mailchimp.List.Member
  ( ListMemberId
  , ListMemberRequest(..)
  , makeListMemberRequest
  , ListMemberResponse(..)
  , ListMemberStatus(..)
  , ListMembersResponse(..)
  )
  where

-- aeson
import Data.Aeson
import qualified Data.Aeson as Aeson

-- mailchimp
import Web.Mailchimp.Common

-- text
import Data.Text (Text)

type ListMemberId =
  Id

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
    , listMemberResponseEmail :: Text
    , listMemberResponseStatus :: ListMemberStatus
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
          <*> o .: "email_address"
          <*> o .: "status"

-- |
--
--

data ListMembersResponse =
  ListMembersResponse
    { listMembersMembers :: [ListMemberResponse]
    }
  deriving (Show)

-- |
--
--

instance FromJSON ListMembersResponse where
  parseJSON =
    Aeson.withObject "" $
      \o ->
        ListMembersResponse
          <$> o .: "members"

-- |
--
--

data ListMemberStatus
  = Cleaned
  | Pending
  | Subscribed
  | Unsubscribed
  deriving (Show, Eq)

-- |
--
--

instance ToJSON ListMemberStatus where
  toJSON Cleaned = "cleaned"
  toJSON Pending = "pending"
  toJSON Subscribed = "subscribed"
  toJSON Unsubscribed = "unsubscribed"

instance FromJSON ListMemberStatus where
  parseJSON =
    Aeson.withText "" $
      \s -> case s of
        "cleaned" -> return Cleaned
        "pending" -> return Pending
        "subscribed" -> return Subscribed
        "unsubscribed" -> return Unsubscribed
        other -> fail $ "expected ListMemberStatus, encountered " ++ show other
