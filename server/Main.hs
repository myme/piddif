{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.FileEmbed (embedFile)
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Parse as Wai
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opts
import qualified Piddif as P
import qualified System.IO as IO

type Handler = Wai.Request -> IO Wai.Response

runServer :: Options -> IO ()
runServer opts = Warp.run opts.port $ \request respond -> do
  let method = Wai.requestMethod request
  response <- case method of
    "GET" -> showForm opts request
    "POST" -> generateDoc request
    _ -> pure $ Wai.responseLBS H.status405 [] "Method Not Allowed"
  putStrLn
    ( show (H.statusCode $ Wai.responseStatus response)
        <> " "
        <> B8.unpack method
        <> " "
        <> B8.unpack (Wai.rawPathInfo request)
    )
  respond response

showForm :: Options -> Handler
showForm opts _ = do
  if opts.embed
    then pure $ Wai.responseLBS H.status200 [(H.hContentType, "text/html")] formEmbed
    else pure $ Wai.responseFile H.status200 [] "./server/form.html" Nothing
  where
    formEmbed = BL8.fromStrict $(embedFile "./server/form.html")

generateDoc :: Handler
generateDoc request = do
  (params, _) <- Wai.parseRequestBodyEx Wai.defaultParseRequestBodyOptions Wai.lbsBackEnd request
  let mode = maybe (Right P.Markdown) (P.modeParser . B8.unpack) $ lookup "mode" params
  let scheme = maybe (Right P.Light) (P.schemeParser . B8.unpack) $ lookup "scheme" params
  let input = maybe (Left "Bad Request: missing input") Right $ lookup "input" params
  case (,,) <$> mode <*> scheme <*> input of
    Left err -> pure $ Wai.responseLBS H.status400 [] (BL8.pack err)
    Right (mode', scheme', input') -> do
      let opts =
            P.Options
              { P._mode = mode',
                P._defaultstyles = True,
                P._css = Nothing,
                P._stylesheet = Nothing,
                P._toc = Nothing,
                P._scheme = Just scheme'
              }
      result <- P.piddif opts (T.decodeUtf8 input')
      let responseBody = BS.fromStrict $ T.encodeUtf8 result
      pure $ Wai.responseLBS H.status200 [(H.hContentType, "text/html")] responseBody

data Options = Options
  { host :: String,
    port :: Int,
    embed :: Bool
  }

main :: IO ()
main = do
  opts <- Opts.execParser $ Opts.info (argParser <**> Opts.helper) Opts.fullDesc
  IO.hSetBuffering IO.stdout IO.LineBuffering
  putStrLn $ "Starting server on http://" <> opts.host <> ":" <> show opts.port
  runServer opts
  where
    argParser =
      Options
        <$> Opts.strOption
          ( Opts.long "host" <> Opts.metavar "host" <> Opts.help "host to bind to" <> Opts.value "localhost"
          )
        <*> Opts.option
          Opts.auto
          ( Opts.long "port" <> Opts.metavar "port" <> Opts.help "port to listen on" <> Opts.value 8000
          )
        <*> ( not
                <$> Opts.switch
                  ( Opts.long "no-embed" <> Opts.help "don't embed the form in the executable"
                  )
            )
