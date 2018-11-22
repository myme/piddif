module Main where

import           Control.Monad (when)
import           Data.Maybe (isJust)
import           Data.Text (unpack)
import qualified Data.Text.IO as T
import           Piddif
import           System.Environment (getArgs)
import           System.IO.Temp (writeSystemTempFile)
import           System.Process (callProcess)

parseArg :: String -> Options -> Options
parseArg arg opts = case arg of
  "-h"         -> opts { _help = True }
  "--help"     -> opts { _help = True }
  "--md"       -> opts { _mode = Markdown }
  "--markdown" -> opts { _mode = Markdown }
  "--org"      -> opts { _mode = Org }
  "--open"     -> opts { _open = True }
  filename     -> if isJust $ _infile opts
    then opts { _outfile = Just filename }
    else opts { _infile = Just filename }

data Options = Options { _help :: Bool
                       , _infile :: Maybe String
                       , _outfile :: Maybe String
                       , _open :: Bool
                       , _mode :: Mode
                       }

defaults :: Options
defaults = Options { _infile = Nothing
                   , _outfile = Nothing
                   , _help = False
                   , _mode = Org
                   , _open = False
                   }

open :: FilePath -> IO ()
open file = do
  putStrLn $ "Opening file: " ++ file
  callProcess "sh" ["-c", "xdg-open " ++ file]

main :: IO ()
main = do
  opts <- foldr parseArg defaults . reverse <$> getArgs
  txt <- case _infile opts of
    Nothing -> T.getContents
    Just filename -> T.readFile filename
  res <- piddif (_mode opts) txt
  case _outfile opts of
    Nothing -> if _open opts
      then do
        file <- writeSystemTempFile "piddif.html" $ unpack res
        open file
      else T.putStrLn res
    Just filename -> do
      T.writeFile filename res
      when (_open opts) $ open filename
