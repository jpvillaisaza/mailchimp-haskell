{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp
--
--
--
----------------------------------------------------------------------

module Web.MailChimp
  ( Client(..)
  , ListClient(..)
  , ListMemberClient(..)
  , makeClient
  , makeListMemberClient
  , ListMemberRequest(..)
  , makeListMemberRequest
  , ListMemberResponse(..)
  , makeManager
  )
  where

-- aeson
import Data.Aeson

-- base
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)

-- bytestring
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (unpack)

-- http-client
import Network.HTTP.Client (Manager)

-- mailchimp
import Web.MailChimp.Extra
import Web.MailChimp.Key
import Web.MailChimp.List
import Web.MailChimp.List.Member

-- servant
import Servant.API

-- servant-client
import Servant.Client hiding (Client)
import qualified Servant.Client as Servant

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)


-- |
--
--

type Api =
  "3.0"
    :> BasicAuth "" Void
    :> (Get '[JSON] Object :<|> ListApi)


-- |
--
-- A client for MailChimp.

data Client =
  Client
    { -- |
      --
      --

      getLinks
        :: forall m . MonadIO m
        => m (Either ServantError Object)

      -- |
      --
      -- Create a client for a list.

    , makeListClient
        :: ListId
        -> ListClient

    }


-- |
--
-- Create a client for MailChimp.

makeClient
  :: Manager -- ^ A manager
  -> Key -- ^ A key
  -> Maybe Client -- ^ The client
makeClient manager key =
  case makeBaseUrl key of
    Nothing ->
      Nothing

    Just baseUrl ->
      let
        makeGetLinks :<|> client' = client (Proxy :: Proxy Api) basicAuthData

        getLinks :: MonadIO m => m (Either ServantError Object)
        getLinks = run (makeGetLinks manager baseUrl)

        makeListClient = makeListClient' manager baseUrl client'

        basicAuthData = BasicAuthData "" key
      in
        Just Client {..}


-- |
--
--

makeListClient'
  :: Manager
  -> BaseUrl
  -> Servant.Client ListApi
  -> ListId
  -> ListClient
makeListClient' manager baseUrl client' listId =
  let
    listMemberClient =
      makeListMemberClient' manager baseUrl (client' listId)
  in
    ListClient {..}


-- |
--
--

makeListMemberClient'
  :: Manager
  -> BaseUrl
  -> Servant.Client ListMemberApi
  -> ListMemberClient
makeListMemberClient' manager baseUrl listClient =
  let
    makeAddListMember
      :<|> md
      :<|> md2
      :<|> md3
      :<|> md4
      :<|> makeDeleteListMember = listClient

    addListMember lm = run (makeAddListMember lm manager baseUrl)

    getListMembers :: MonadIO m => m (Either ServantError [ListMemberResponse])
    getListMembers = run (md manager baseUrl)

    getListMember s = run (md2 s manager baseUrl)

    updateListMember s lm = run (md3 s lm manager baseUrl)

    addOrUpdateListMember s lm = run (md4 s lm manager baseUrl)

    deleteListMember s = run (makeDeleteListMember s manager baseUrl)
  in
    ListMemberClient {..}


-- |
--
--

run
  :: MonadIO m
  => ExceptT e IO a
  -> m (Either e a)
run =
  liftIO . runExceptT


-- |
--
-- Create a client for a list members.

makeListMemberClient
  :: Manager
  -> Key
  -> String
  -> Maybe ListMemberClient
makeListMemberClient manager key listId =
  listMemberClient . (`makeListClient` listId)
    <$> makeClient manager key

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
