module Main where

import OptParse
import qualified HsBlog

import System.Exit ( exitFailure )
import System.Directory ( doesFileExist )
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    -- TODO take flag into account
    ConvertDir input output replace ->
      HsBlog.convertDirectory input output

    ConvertSingle input output replace ->
      let
        withInputHandle :: (String -> Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin ->
              action "" stdin
            InputFile file ->
              withFile file ReadMode (action file)

        withOutputHandle :: (Handle -> IO a) -> IO a
        withOutputHandle action =
          case output of
            Stdout ->
              action stdout
            OutputFile file -> do
              exists <- doesFileExist file
              shouldOpenFile <-
                if exists && not replace
                  then confirm
                  else pure True
              if shouldOpenFile
                then
                  withFile file WriteMode action
                else
                  exitFailure
      in
        withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response. use y or n" *> confirm
