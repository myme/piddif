{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Parse as Wai
import qualified Piddif as P

runServer :: Warp.Port -> IO ()
runServer port = Warp.run port $ \request respond -> do
  let method = Wai.requestMethod request
  case method of
    "GET" -> showForm request respond
    "POST" -> generateDoc request respond
    _ -> respond $ Wai.responseLBS H.status405 [] "Method Not Allowed"

showForm :: Wai.Application
showForm _ respond = do
  respond $ Wai.responseFile H.status200 [(H.hContentType, "text/html")] "./server/form.html" Nothing

generateDoc :: Wai.Application
generateDoc request respond = do
  (params, _) <- Wai.parseRequestBodyEx Wai.defaultParseRequestBodyOptions Wai.lbsBackEnd request
  let scheme = maybe (Right P.Light) (P.schemeParser . B8.unpack) $ lookup "scheme" params
  case scheme of
    Left err -> respond $ Wai.responseLBS H.status400 [] (BL8.pack err)
    Right scheme' -> case lookup "input" params of
      Nothing -> respond $ Wai.responseLBS H.status400 [] "Bad Request: missing input"
      Just input -> do
        let opts =
              P.Options
                { P._mode = P.Org,
                  P._defaultstyles = True,
                  P._css = Nothing,
                  P._stylesheet = Nothing,
                  P._toc = Nothing,
                  P._scheme = Just scheme'
                }
        result <- P.piddif opts (T.decodeUtf8 input)
        let responseBody = BS.fromStrict $ T.encodeUtf8 result
        respond $ Wai.responseLBS H.status200 [(H.hContentType, "text/html")] responseBody

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Starting server on http://localhost:" <> show port
  runServer port
