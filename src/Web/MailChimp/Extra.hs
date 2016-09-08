----------------------------------------------------------------------
-- |
-- Module: Web.MailChimp
--
--
--
----------------------------------------------------------------------

module Web.MailChimp.Extra
  ( makeManager
  )
  where

-- http-client
import Network.HTTP.Client (Manager, newManager)

-- http-client-tls
import Network.HTTP.Client.TLS (tlsManagerSettings)


-- |
--
-- Create a manager.

makeManager :: IO Manager
makeManager =
  newManager tlsManagerSettings
