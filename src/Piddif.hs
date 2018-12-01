{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Piddif
    ( Mode(..)
    , normalizeHeadlines
    , piddif
    ) where

import           Data.FileEmbed (embedStringFile)
import qualified Data.Text as T
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Pandoc (getDefaultExtensions, readerExtensions, PandocIO, readMarkdown, writeHtml5, runIOorExplode, def, readOrg)

renderHtml :: H.Html -> PandocIO String
renderHtml markup = return $ renderMarkup $ do
  H.docType
  H.html $ do
    H.head $ do
      H.title "Piddif rendered HTML"
      H.meta ! A.charset "utf-8"
      H.style ! A.type_ "text/css" $ $(embedStringFile "./src/default.css")
    H.body markup

data Mode = Markdown | Org

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

piddif :: Mode -> T.Text -> IO T.Text
piddif mode txt = do
  let generate = case mode of
        Markdown -> readMarkdown mdOpts
        Org -> readOrg def
  let normalized = normalizeHeadlines mode txt
  T.pack <$> runIOorExplode (pure normalized >>= generate >>= writeHtml5 def >>= renderHtml)
  where mdOpts = def { readerExtensions = getDefaultExtensions "gfm" }
