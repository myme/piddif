{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Piddif
import Test.Hspec

simpleOrg :: Text
simpleOrg = "*** Third level\n\
            \***Not a header\n\
            \**** Fourth level\n\
            \** Second level\n"

simpleMd :: Text
simpleMd = "### Third level\n\
           \###Not a header\n\
           \#### Fourth level\n\
           \## Second level\n"

main :: IO ()
main = hspec $ do
  describe "Markdown" $
    it "normalizes markdown headlines" $
      normalizeHeadlines Markdown simpleMd `shouldBe` "## Third level\n\
                                                      \###Not a header\n\
                                                      \### Fourth level\n\
                                                      \# Second level\n"

  describe "Org" $
    it "normalizes org headlines" $
      normalizeHeadlines Org simpleOrg `shouldBe` "** Third level\n\
                                                  \***Not a header\n\
                                                  \*** Fourth level\n\
                                                  \* Second level\n"
