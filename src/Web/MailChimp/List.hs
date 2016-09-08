{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp.List
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
import Web.MailChimp.List.Member

-- servant
import Servant.API


-- |
--
--

type ListId =
  String


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
--

data ListClient =
  ListClient
    { -- |
      --
      --

      listMemberClient :: ListMemberClient

    }
