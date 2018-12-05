module Main where

import           Control.Applicative ((<|>), (<**>), optional)
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

openFile :: FilePath -> IO ()
openFile file = do
  putStrLn $ "Opening file: " ++ file
  callProcess "sh" ["-c", "xdg-open " ++ file]

main :: IO ()
main = do
  Options infile outfile open piddif <-
      Opts.execParser $ Opts.info (argParser <**> Opts.helper) Opts.fullDesc

  txt <- maybe T.getContents T.readFile infile
  res <- P.piddif piddif txt

  fname  <- case outfile of
    Just filename -> T.writeFile filename res >> pure (Just filename)
    Nothing | open -> Just <$> writeSystemTempFile "piddif.html" (unpack res)
    Nothing -> T.putStrLn res >> pure Nothing

  case fname of
      Just f | open -> openFile f
      _ -> pure ()
