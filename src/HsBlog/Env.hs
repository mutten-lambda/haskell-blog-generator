module HsBlog.Env where

data Env
  = Env
    { eBlogName :: String
    , eStylesheetPath :: FilePath
    }
    deriving Show

defaultEnv :: Env
defaultEnv = Env "My blog" "style.css"
