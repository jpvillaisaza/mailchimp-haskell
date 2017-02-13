{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  ( Key
  , MainClient(..)
  , makeMainClient
  , AuthClient (..)
  , makeAuthClientWithKey
  , ListClient(..)
  , ListId
  , ListMemberClient(..)
  , ListMemberRequest(..)
  , makeListMemberRequest
  , ListMemberResponse(..)
  , ListMemberId
  , ListMemberStatus(..)
  , Id
  , Paths_mailchimp.version
  , makeManager
  , run
  )
  where

-- aeson
import Data.Aeson

-- base
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)
import GHC.Generics

-- bytestring
import Data.ByteString.Char8 (unpack)

-- generics-sop
import Generics.SOP

-- http-client
import Network.HTTP.Client (Manager)

-- mailchimp
import qualified Paths_mailchimp
import Web.MailChimp.Common
import Web.MailChimp.Extra
import Web.MailChimp.Key
import Web.MailChimp.List
import Web.MailChimp.List.Member

-- servant
import Servant.API

-- servant-client
import Servant.Client
import Servant.Client.Generic

-- transformers
import Control.Monad.IO.Class


-- |
--
-- The MailChimp API, version 3.0.

type MainApi =
  "3.0"
    :> BasicAuth "" Void
    :> AuthApi


-- |
--
--

newtype MainClient =
  MainClient
    { makeAuthClient
        :: BasicAuthData
        -> AuthClient

    }
  deriving (GHC.Generics.Generic)


-- |
--
--
instance Generics.SOP.Generic MainClient


-- |
--
--

instance (Client MainApi ~ client) => ClientLike client MainClient


-- |
--
--

makeMainClient :: MainClient
makeMainClient =
  mkClient (client (Proxy :: Proxy MainApi))


-- |
--
--

type AuthApi =
    Get '[JSON] Object
  :<|>
    "lists"
      :> ListApi
  :<|>
    "lists"
      :> Capture "list_id" ListId
      :> "members"
      :> ListMemberApi


-- |
--
-- A client for MailChimp.

data AuthClient =
  AuthClient
    { -- |
      --
      --

      getLinks
        :: ClientM Object

      -- |
      --
      -- Create a client for lists.

    , makeListClient
        :: ListClient

      -- |
      --
      -- Create a client for a list.

    , makeListMemberClient
        :: ListId
        -> ListMemberClient

    }
  deriving (GHC.Generics.Generic)


-- |
--
--

instance Generics.SOP.Generic AuthClient


-- |
--
--

instance (Client AuthApi ~ client) => ClientLike client AuthClient


-- |
--
--

makeAuthClientWithKey
  :: Key
  -> AuthClient
makeAuthClientWithKey key =
  let
    MainClient {..} = makeMainClient
  in
    makeAuthClient (BasicAuthData "" key)



-- |
--
--

run
  :: MonadIO m
  => Manager
  -> Key
  -> ClientM a
  -> m (Either ServantError a)
run manager key =
  let
    Just baseUrl =
      makeBaseUrl key
  in
    liftIO . flip runClientM (ClientEnv manager baseUrl)


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
