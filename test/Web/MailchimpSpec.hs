{-# LANGUAGE OverloadedStrings #-}

module Web.MailchimpSpec
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
import Web.Mailchimp


main :: IO ()
main =
  hspec spec


spec :: Spec
spec =
  describe "makeBaseUrl" $ do
    it "makes a base url" $ do
      makeBaseUrl "01234567890123456789012345678901-ab01"
        `shouldSatisfy` isJust
