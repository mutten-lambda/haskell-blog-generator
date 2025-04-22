module Main where

import OptParse
import qualified HsBlog

import System.Exit ( exitFailure )
import System.Directory ( doesFileExist )
import System.IO
import HsBlog.Env (defaultEnv)

main :: IO ()
main = do
  options <- parse
  let env = defaultEnv 
  case options of
    ConvertDir input output replace ->
      HsBlog.convertDirectory env replace input output

    ConvertSingle input output replace ->
      let
        withInputHandle :: (Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin ->
              action stdin
            InputFile file ->
              withFile file ReadMode action

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
        withInputHandle $ withOutputHandle . HsBlog.convertSingle env

-- TODO code duplication; see HsBlog.Directory
confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response. use y or n" *> confirm
