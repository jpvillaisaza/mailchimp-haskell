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
-- Module: Web.MailChimp
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.MailChimp
  ( Routes(..)
  , ListRoutes(..)
  , listClient
  , ListMemberRoutes(..)
  , listMemberClient
  , Paths_mailchimp.version
  , getAllListMembers
  , makeBasicAuthData
  , run
  , makeBaseUrl
  , module X
  )
  where

-- aeson
import Data.Aeson

-- base
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)

-- bytestring
import Data.ByteString.Char8 (unpack)

-- http-client
import Network.HTTP.Client (Manager)

-- mailchimp
import qualified Paths_mailchimp
import Web.MailChimp.Common as X
import Web.MailChimp.Extra as X
import Web.MailChimp.Key as X
import Web.MailChimp.List as X
import Web.MailChimp.List.Member as X

-- servant
import Servant.API
import Servant.API.Generic

-- servant-client
import Servant.Client
import Servant.Client.Generic

-- transformers
import Control.Monad.IO.Class

-- |
--
-- The MailChimp API, version 3.0.

data Routes route =
  Routes
    { getLinks :: route
        :- BasicAuth "" Void
        :> Get '[JSON] Object
    , list :: route :- ToServantApi ListRoutes
    , listMember :: route :- ToServantApi ListMemberRoutes
    }
  deriving Generic

type Api = "3.0" :> ToServantApi Routes

api :: Proxy Api
api = Proxy

mainClient :: Routes (AsClientT ClientM)
mainClient = fromServant (client api)

data ListRoutes route =
  List
    { getLists :: route
        :- BasicAuth "" Void
        :> "lists"
        :> Get '[JSON] [String]
    }
  deriving Generic

listClient :: ListRoutes (AsClientT ClientM)
listClient = fromServant (list mainClient)

data ListMemberRoutes route =
  ListMember
    { -- | Add a new list member.
      addListMember :: route
        :- BasicAuth "" Void
        :> "lists"
        :> Capture "list_id" ListId
        :> "members"
        :> ReqBody '[JSON] ListMemberRequest
        :> Post '[JSON] ListMemberResponse
      -- | Get information about members in a list.
    , getListMembers :: route
        :- BasicAuth "" Void
        :> "lists"
        :> Capture "list_id" ListId
        :> "members"
        :> QueryParam "offset" Int
        :> Get '[JSON] ListMembersResponse
      -- | Get information about a specific list member.
    , getListMember :: route :-
        BasicAuth "" Void
        :> "lists"
        :> Capture "list_id" ListId
        :> "members"
        :> Capture "subscriber_hash" ListMemberId
        :> Get '[JSON] ListMemberResponse
      -- | Update a list member.
    , updateListMember :: route
        :- BasicAuth "" Void
        :> "lists"
        :> Capture "list_id" ListId
        :> "members"
        :> Capture "subscriber_hash" ListMemberId
        :> ReqBody '[JSON] ListMemberRequest
        :> Patch '[JSON] ListMemberResponse
      -- | Add or update a list member.
    , addOrUpdateListMember :: route
        :- BasicAuth "" Void
        :> "lists"
        :> Capture "list_id" ListId
        :> "members" :> Capture "subscriber_hash" ListMemberId
        :> ReqBody '[JSON] ListMemberRequest
        :> Put '[JSON] ListMemberResponse
      -- | Remove a list member.
    , deleteListMember :: route
        :- BasicAuth "" Void
        :> "lists"
        :> Capture "list_id" ListId
        :> "members"
        :> Capture "subscriber_hash" ListMemberId
        :> Delete '[JSON] String
    }
  deriving Generic

listMemberClient :: ListMemberRoutes (AsClientT ClientM)
listMemberClient = fromServant (listMember mainClient)

-- |
--
--
--
-- @since 0.3.0

getAllListMembers :: BasicAuthData -> ListId -> ClientM [ListMemberResponse]
getAllListMembers basicAuthData listId = do
  xs <- listMembersMembers <$> getListMembers listMemberClient basicAuthData listId Nothing
  rest <- go 0 (length xs)
  return $ xs ++ rest
  where
    go :: Int -> Int -> ClientM [ListMemberResponse]
    go _ 0 = return []
    go offset n = do
      xs <- listMembersMembers <$> getListMembers listMemberClient basicAuthData listId (Just $ offset + n)
      rest <- go (offset + n) (length xs)
      return $ xs ++ rest

makeBasicAuthData :: Key -> BasicAuthData
makeBasicAuthData = BasicAuthData ""

-- |
--
--

run
  :: MonadIO m
  => Manager
  -> Key
  -> ClientM a
  -> m (Either ClientError a)
run manager key =
  let
    Just baseUrl =
      makeBaseUrl key
  in
    liftIO . flip runClientM (mkClientEnv manager baseUrl)

-- |
--
--

makeBaseUrl
  :: Key
  -> Maybe BaseUrl
makeBaseUrl key =
  case fmap unpack (parseDataCenter key) of
    Left _ ->
      Nothing

    Right dataCenter ->
      Just $
        BaseUrl
          Https
          (mappend dataCenter ".api.mailchimp.com")
          443
          ""
