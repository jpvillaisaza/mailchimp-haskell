{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.List
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.List
  ( ListApi
  , ListClient (..)
  , ListId
  )
  where

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


-- |
--
-- A list ID.

type ListId =
  Id


-- |
--
--

type ListApi =
    Get '[JSON] [String]


-- |
--
-- A client for a list.

data ListClient =
  ListClient
    { -- |
      --
      --

      getLists :: ClientM [String]

    }
  deriving (GHC.Generics.Generic)


-- |
--
--
instance Generics.SOP.Generic ListClient


-- |
--
--

instance (Client ListApi ~ client) => ClientLike client ListClient
