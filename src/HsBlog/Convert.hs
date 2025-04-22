module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Env

convert :: Env -> String -> Markup.Document -> Html.Html
convert env title doc = 
  let
    header = 
      Html.title_ (eBlogName env <> " - " <> title) 
        <> Html.stylesheet_ (eStylesheetPath env)
    article =
      foldMap convertStructure doc 
    fullTitle =
      Html.h_ 1 (Html.link_ "index.html" . Html.txt_ $ eBlogName env)
    body =
      fullTitle <> article
  in 
    Html.html_ header body

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt

    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list

    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list

    Markup.CodeBlock list ->
      Html.pre_ (unlines list)
