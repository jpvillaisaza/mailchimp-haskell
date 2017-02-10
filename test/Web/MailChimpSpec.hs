{-# LANGUAGE OverloadedStrings #-}

module Web.MailChimpSpec
  ( main
  , spec
  )
  where

-- base
import Control.Monad
import Data.Maybe

-- hspec
import Test.Hspec

-- mailchimp
import Web.MailChimp


main :: IO ()
main =
  hspec spec


spec :: Spec
spec =
  describe "makeClient" $ do
    it "" $ do
      manager <- makeManager
      let
        maybeClient =
          makeClient manager mempty
      unless (isNothing maybeClient) $
        pending

    it "" $ do
      manager <- makeManager
      let
        maybeClient =
          makeClient manager "01234567890123456789012345678901-ab01"
      unless (isJust maybeClient) $
        pending
