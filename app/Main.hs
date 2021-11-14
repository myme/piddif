module Main where

import           Control.Applicative (Alternative, (<|>), (<**>), empty, optional)
import           Data.Maybe (fromMaybe)
import           Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Options.Applicative as Opts
import qualified Piddif as P
import           System.Exit (die)
import           System.FilePath (takeExtension)
import           System.IO.Temp (writeSystemTempFile)
import           System.Process (callProcess)

data Options = Options { _infile :: Maybe String
                       , _outfile :: Maybe String
                       , _open :: Bool
                       , _mode :: Maybe P.Mode
                       , _defaultStyles :: Bool
                       , _inlineStyles :: Maybe String
                       , _customStylesheet :: Maybe String
                       , _toc :: Maybe Int
                       }

modeParser :: Opts.Parser (Maybe P.Mode)
modeParser = optional
   $ Opts.flag' P.Markdown (Opts.long "md" <>
                            Opts.long "markdown" <>
                            Opts.help "parse input as markdown")
  <|> Opts.flag' P.Org (Opts.long "org" <>
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
  <*> modeParser
  <*> (not <$> Opts.switch (
        Opts.short 'C' <>
        Opts.long "no-css" <>
        Opts.help "no default styles"))
  <*> optional (Opts.strOption (
                 Opts.long "css" <>
                 Opts.metavar "css" <>
                 Opts.help "inline styles"))
  <*> optional (Opts.strOption (
                 Opts.long "stylesheet" <>
                 Opts.metavar "stylesheet" <>
                 Opts.help "custom stylesheet"))
  <*> optional (Opts.option Opts.auto (
                   Opts.long "toc" <>
                   Opts.help "add table of contents"))

openFile :: FilePath -> IO ()
openFile file = do
  putStrLn $ "Opening file: " ++ file
  callProcess "sh" ["-c", "xdg-open " ++ file]

guessMode :: Options -> Either String P.Mode
guessMode opts =
  fromMaybe (Left "Cannot guess mode: provide --md or --org")
         $  (Right <$> _mode opts)
        <|> (fromExtension <$> _infile opts)
  where
    fromExtension fname = let ext = takeExtension fname in case ext of
      ".md"  -> Right P.Markdown
      ".org" -> Right P.Org
      _      -> Left $ "Unknown extension: " <> ext

emptyIf :: Alternative f => (a -> Bool) -> a -> f a
emptyIf f x = if f x then empty else pure x

main :: IO ()
main = do
  opts <- Opts.execParser $ Opts.info (argParser <**> Opts.helper) Opts.fullDesc
  case guessMode opts of
    Left err -> die err
    Right mode -> do
      txt <- maybe T.getContents T.readFile (_infile opts >>= emptyIf ("-" ==))
      let pOpts = P.Options mode (_defaultStyles opts) (_inlineStyles opts) (_customStylesheet opts) (_toc opts)
      res <- P.piddif pOpts txt

      fname <- case _outfile opts of
        Just filename -> T.writeFile filename res >> pure (Just filename)
        Nothing | _open opts -> Just <$> writeSystemTempFile "piddif.html" (unpack res)
        Nothing -> T.putStrLn res >> pure Nothing

      case fname of
          Just f | _open opts -> openFile f
          _ -> pure ()
