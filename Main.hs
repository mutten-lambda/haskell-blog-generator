module Main where

import qualified Markup
import qualified Html
import Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= putStrLn . process "???" 
    [input,output] -> do
      let writeResult = readFile input >>= writeFile output . process "???" 
      fileExists <- doesFileExist input 
      if fileExists
      then whenIO (confirm "This file already exists. Continue y/n?") writeResult
      else writeResult
    _ -> putStrLn "Usage: runghc Main.hs [<input-file> <output-file>]"

confirm :: String -> IO Bool
confirm str = do
  putStrLn str
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response. Use y or n." >> confirm str

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result then action else pure ()

