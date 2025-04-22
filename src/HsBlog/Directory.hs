module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html.Internal as Html
    ( Html, html_, p_, h_, txt_, link_, render )
import HsBlog.Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException (..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )
import HsBlog.Env

convertDirectory :: Env -> Bool -> FilePath -> FilePath -> IO ()
convertDirectory env replace inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit replace outputDir
  let
    outputHtmls = txtsToRenderedHtml env filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
    , dcFilesToCopy :: [FilePath]
    }

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    { dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action list =
  for list $ \input -> do
  maybeResult <-
    catch
      (Right <$> action input)
      (\(SomeException e) -> do
        pure $ Left (displayException e)
      )
  return (input, maybeResult)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(a,contentOrError) ->
    case contentOrError of
      Right b -> return [(a,b)]
      Left str -> hPutStrLn stderr str >> return []

createOutputDirectoryOrExit :: Bool -> FilePath -> IO ()
createOutputDirectoryOrExit replace outputDir = do
  dirCreated <- createOutputDirectory replace outputDir
  when (not dirCreated) (hPutStrLn stderr "Cancelled." >> exitFailure)

createOutputDirectory :: Bool -> FilePath -> IO Bool
createOutputDirectory replace dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists && not replace
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create

txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml env txts = 
  let
    markup = map toOutputMarkupFile txts
    index = ("index", buildIndex defaultEnv markup)
    htmlContent = map (convertFile env) markup
  in 
    map (fmap Html.render) (index : htmlContent)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (path, str) = (takeBaseName path <.> "html", Markup.parse str)

convertFile :: Env -> (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile env (path, content) = (path , convert env content)

buildIndex :: Env -> [(FilePath, Markup.Document)] -> Html.Html
buildIndex env files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Heading 1 heading : article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                <> foldMap convertStructure (take 2 article)
                <> Html.p_ (Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      (eBlogName env)
      ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n."
      confirm question
