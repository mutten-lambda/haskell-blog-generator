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
    ConvertDir input output replace env ->
      HsBlog.convertDirectory env replace input output

    ConvertSingle input output replace -> do
      (title, inputHandle) <-
        case input of 
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode 
      outputHandle <-
        case output of
          Stdout -> 
            pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists && not replace
                then confirm
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode 
              else
                exitFailure
      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

-- TODO code duplication; see HsBlog.Directory
confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response. use y or n" *> confirm
