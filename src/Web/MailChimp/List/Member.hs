{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.Member
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
  )
  where

-- aeson
import Data.Aeson
import qualified Data.Aeson as Aeson

-- servant
import Servant.API

-- servant-client
import Servant.Client

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.IO.Class (MonadIO)


-- |
--
--

type ListMemberApi =
       ReqBody '[JSON] ListMemberRequest
         :> Post '[JSON] ListMemberResponse

  :<|> Get '[JSON] [ListMemberResponse]

  :<|> Capture "subscriber_hash" String
         :> Get '[JSON] ListMemberResponse

  :<|> Capture "subscriber_hash" String
         :> ReqBody '[JSON] ListMemberRequest
         :> Patch '[JSON] ListMemberResponse

  :<|> Capture "subscriber_hash" String
         :> ReqBody '[JSON] ListMemberRequest
         :> Put '[JSON] ListMemberResponse


  :<|> Capture "subscriber_hash" String
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
        => String
        -> m (Either ServantError ListMemberResponse)

      -- |
      --
      -- Update a list member.

    , updateListMember
        :: forall m . MonadIO m
        => String
        -> ListMemberRequest
        -> m (Either ServantError ListMemberResponse)

      -- |
      --
      -- Add or update a list member.

    , addOrUpdateListMember
        :: forall m . MonadIO m
        => String
        -> ListMemberRequest
        -> m (Either ServantError ListMemberResponse)

      -- |
      --
      -- Remove a list member.

    , deleteListMember
        :: forall m . MonadIO m
        => String
        -> m (Either ServantError String)

    }


-- |
--
--

data ListMemberRequest =
  ListMemberRequest
    { listMemberEmailAddress :: String
    , listMemberMergeFields :: [(Text, Text)]
    , listMemberStatus :: String
    , listMemberExtra :: [(Text, Aeson.Value)]
    }
  deriving (Show)


-- |
--
--

makeListMemberRequest
  :: String
  -> String
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
      [ "email_address" .= listMemberEmailAddress
      , ("merge_fields", Aeson.object (fmap (fmap Aeson.toJSON) listMemberMergeFields))
      , "status" .= listMemberStatus
      ]
      `mappend` listMemberExtra

-- |
--
--

data ListMemberResponse =
  ListMemberResponse
    { listMemberId :: Text
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
