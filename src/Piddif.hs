{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Piddif
    ( Mode(..)
    , Options(..)
    , normalizeHeadlines
    , piddif
    ) where

import           Control.Arrow ((>>>))
import           Control.Monad ((>=>))
import           Data.FileEmbed (embedStringFile)
import qualified Data.Text as T
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Pandoc (getDefaultExtensions, readerExtensions, PandocIO, readMarkdown, writeHtml5, runIOorExplode, def, readOrg)

data Mode = Markdown | Org

data Options = Options { _mode :: Mode
                       , _stylesheet :: Maybe String
                       }

defaultCss :: H.Html
defaultCss = $(embedStringFile "./src/default.css")

renderHtml :: Options -> H.Html -> PandocIO String
renderHtml opts markup = return $ renderMarkup $ do
  H.docType
  H.html $ do
    H.head $ do
      H.title "Piddif rendered HTML"
      H.meta ! A.charset "utf-8"
      case _stylesheet opts of
        Nothing -> H.style ! A.type_ "text/css" $ defaultCss
        Just css -> H.link ! A.rel "stylesheet" ! A.href (H.stringValue css)
    H.body markup

normalizeHeadlines :: Mode -> T.Text -> T.Text
normalizeHeadlines mode input = T.unlines . map liftHeader $ ls
  where ls = T.lines input
        headerChar = case mode of
          Markdown -> '#'
          Org -> '*'
        headerLevel line = case T.span (== headerChar) line of
          (prefix, rest) | T.null prefix || not (T.isPrefixOf " " rest) -> 0
                         | otherwise -> T.length prefix
        isHeader = (> 0) . headerLevel
        toplevel = minimum $ map headerLevel $ filter isHeader ls
        liftHeader line | isHeader line = T.drop (toplevel - 1) line
                        | otherwise = line

piddif :: Options -> T.Text -> IO T.Text
piddif opts@(Options mode _) =
  let
      generate Markdown = readMarkdown $ def { readerExtensions = getDefaultExtensions "gfm" }
      generate Org      = readOrg def
      normalize = normalizeHeadlines mode
      process = normalize >>> generate mode >=> writeHtml5 def >=> renderHtml opts
  in
  (T.pack <$>) . runIOorExplode . process
