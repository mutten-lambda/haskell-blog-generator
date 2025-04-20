module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

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

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex docs = 
  let
    makeIndexLink path doc = (Html.p_ . Html.link_ path) (Html.txt_ . firstHeading $ doc)
  in 
    Html.html_ "Index" (Html.ul_ . map (uncurry makeIndexLink) $ docs)

firstHeading :: Markup.Document -> String
firstHeading [] = ""
firstHeading ((Markup.Heading _ title):_) = title
firstHeading (_:xs) = firstHeading xs
