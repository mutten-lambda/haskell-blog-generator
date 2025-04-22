module HsBlog 
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)
import HsBlog.Directory

import System.IO ( Handle, hGetContents, hPutStrLn )
import HsBlog.Env

convertSingle :: Env -> Handle -> Handle -> IO ()
convertSingle env input output = do
  content <- hGetContents input
  hPutStrLn output (process env content)

process :: Env -> String -> String
process env = Html.render . convert env . Markup.parse

