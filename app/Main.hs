module Main where

import qualified Data.Text.IO as T
import           Piddif
import           System.Environment (getArgs)

parseArg :: String -> Options -> Options
parseArg arg opts = case arg of
  "-h"         -> opts { _help = True }
  "--help"     -> opts { _help = True }
  "--md"       -> opts { _mode = Markdown }
  "--markdown" -> opts { _mode = Markdown }
  "--org"      -> opts { _mode = Org }
  filename     -> opts { _infile = Just filename }

data Options = Options { _help :: Bool
                       , _infile :: Maybe String
                       , _mode :: Mode }

defaults :: Options
defaults = Options { _infile = Nothing, _help = False, _mode = Org }

main :: IO ()
main = do
  opts <- foldr parseArg defaults <$> getArgs
  txt <- case _infile opts of
    Nothing -> T.getContents
    Just filename -> T.readFile filename
  res <- piddif (_mode opts) txt
  T.putStrLn res
