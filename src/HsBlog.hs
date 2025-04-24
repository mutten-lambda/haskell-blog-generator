{-|
Module      : HsBlog
Description : HsBlog is a static site generator written in Haskell
|-}
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
import HsBlog.Env (defaultEnv)

-- | Convert a single file in the custom markup language to an rendered html file.
convertSingle :: String -- ^ The title
              -> Handle -- ^ The input handle
              -> Handle -- ^ The output handle
              -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

-- | Convert markup text to html.
process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse

