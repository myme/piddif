{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Piddif
    ( Mode(..)
    , Options(..)
    , normalizeHeadlines
    , piddif
    ) where

import           Control.Arrow ((>>>))
import           Control.Monad ((>=>), when)
import           Data.FileEmbed (embedStringFile)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.String (renderMarkup)
import qualified Text.Pandoc as Pandoc
import           Text.Pandoc
  ( getDefaultExtensions
  , readerExtensions
  , pandocExtensions
  , PandocIO
  , readMarkdown
  , writeHtml5
  , runIOorExplode
  , def
  , readOrg
  , writerTableOfContents
  , writerTOCDepth
  , writerTemplate
  )

data Mode = Markdown | Org

data Options = Options { _mode :: Mode
                       , _defaultstyles :: Bool
                       , _css :: Maybe String
                       , _stylesheet :: Maybe String
                       , _toc :: Maybe Int
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
      when (_defaultstyles opts) (renderCss defaultCss)
      maybe (pure mempty) (renderCss . H.toHtml) (_css opts)
      maybe (pure mempty) renderCssLink (_stylesheet opts)
    H.body markup
  where renderCss css = H.style ! A.type_ "text/css" $ css
        renderCssLink url = H.link ! A.rel "stylesheet" ! A.href (H.stringValue url)

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
piddif opts =
  let
    mode = _mode opts
    generate Markdown = readMarkdown def { readerExtensions = getDefaultExtensions "markdown" }
    generate Org      = readOrg def { readerExtensions = pandocExtensions }
    normalize = normalizeHeadlines mode
    withToc = isJust $ _toc opts
    tocTemplate = if withToc
      then Just $ either error id $ either (error . show) id $
        Pandoc.runPure $ Pandoc.runWithDefaultPartials $
        Pandoc.compileTemplate "" "<div class=\"toc\"><h1>Contents</h1>\n$toc$\n</div>\n$body$"
      else Nothing
    writerOptions = def { writerTableOfContents = withToc
                        , writerTOCDepth = fromMaybe 0 $ _toc opts
                        , writerTemplate = tocTemplate
                        }
    process = normalize >>> generate mode >=> writeHtml5 writerOptions >=> renderHtml opts
  in
    (T.pack <$>) . runIOorExplode . process
