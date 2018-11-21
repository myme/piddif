{-# LANGUAGE OverloadedStrings #-}

module Piddif
    ( Mode(..)
    , piddif
    ) where

import           Data.Text (pack, Text)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Pandoc (PandocIO, readMarkdown, writeHtml5, runIOorExplode, def, readOrg)

renderHtml :: H.Html -> PandocIO String
renderHtml markup = return $ renderMarkup $ do
  H.docType
  H.html $ do
    H.head $ do
      H.title "Piddif rendered HTML"
      H.meta ! A.charset "utf-8"
      H.link
        ! A.href "https://fonts.googleapis.com/css?family=Roboto+Mono|Roboto:400,500"
        ! A.rel "stylesheet"
      H.link
        ! A.href "https://use.fontawesome.com/releases/v5.0.2/css/all.css"
        ! A.rel "stylesheet"
      H.link
        ! A.href "https://myme.no/css/default.css"
        ! A.rel "stylesheet"
    H.body markup

data Mode = Markdown | Org

piddif :: Mode -> Text -> IO Text
piddif mode txt = do
  let generate = case mode of
        Markdown -> readMarkdown def
        Org -> readOrg def
  pack <$> runIOorExplode (generate txt >>= writeHtml5 def >>= renderHtml)
