{-# LANGUAGE DataKinds #-}
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
  , ListClient(..)
  , ListId
  )
  where

-- mailchimp
import Web.MailChimp.Common
import Web.MailChimp.List.Member

-- servant
import Servant.API


-- |
--
-- A list ID.

type ListId =
  Id


-- |
--
--

type ListApi =
  "lists"
    :> Capture "list_id" ListId
    :> "members"
    :> ListMemberApi


-- |
--
-- A client for a list.

data ListClient =
  ListClient
    { -- |
      --
      --

      listMemberClient :: ListMemberClient

    }
