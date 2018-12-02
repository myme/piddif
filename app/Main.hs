module Main where

import           Control.Applicative ((<|>), (<**>), optional)
import           Control.Monad (when)
import           Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Options.Applicative as Opts
import qualified Piddif as P
import           System.IO.Temp (writeSystemTempFile)
import           System.Process (callProcess)

data Options = Options { _infile :: Maybe String
                       , _outfile :: Maybe String
                       , _open :: Bool
                       , _piddif :: P.Options
                       }

modeParser :: Opts.Parser P.Mode
modeParser = (
  Opts.flag' P.Markdown (Opts.long "md" <>
                         Opts.help "parse input as markdown") <|>
  Opts.flag' P.Markdown (Opts.long "markdown" <>
                         Opts.help "parse input as markdown")
  ) <|>
  Opts.flag P.Org P.Org (Opts.long "org" <>
                         Opts.help "parse input as org-mode")

argParser :: Opts.Parser Options
argParser = Options
  <$> optional (Opts.strArgument (
                   Opts.metavar "infile" <>
                   Opts.help "defaults to stdin"))
  <*> optional (Opts.strArgument (
                   Opts.metavar "outfile" <>
                   Opts.help "defaults to stdout"))
  <*> Opts.switch (Opts.short 'o' <>
                   Opts.long "open" <>
                   Opts.help "open result in a browser")
  <*> (P.Options <$>
       modeParser <*>
       optional (Opts.strOption (
                    Opts.long "css" <>
                    Opts.metavar "stylesheet" <>
                    Opts.help "custom stylesheet")))

open :: FilePath -> IO ()
open file = do
  putStrLn $ "Opening file: " ++ file
  callProcess "sh" ["-c", "xdg-open " ++ file]

main :: IO ()
main = do
  opts <- Opts.execParser $ Opts.info (argParser <**> Opts.helper) Opts.fullDesc
  txt <- case _infile opts of
    Nothing -> T.getContents
    Just filename -> T.readFile filename
  res <- P.piddif (_piddif opts) txt
  case _outfile opts of
    Nothing -> if _open opts
      then do
        file <- writeSystemTempFile "piddif.html" $ unpack res
        open file
      else T.putStrLn res
    Just filename -> do
      T.writeFile filename res
      when (_open opts) $ open filename
