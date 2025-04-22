module HsBlog.Html.Internal where

import GHC.Natural (Natural)
import Prelude hiding (head)

newtype Html
  = Html String

newtype Structure
  = Structure String

newtype Content
  = Content String

newtype Head
  = Head String

type Title
  = String

html_ :: Head -> Structure -> Html
html_ (Head head) (Structure content) =
  Html
  . el "html"
  $ el "head" head <> el "body" content

-- * Structure

instance Semigroup Structure
  where
    (Structure a) <> (Structure b) = Structure $ a <> b

instance Monoid Structure where
  mempty = Structure mempty

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

pre_ :: String -> Structure
pre_ = Structure . el "pre" . escape

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString) 

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString) 

-- * Content

instance Semigroup Content
  where
    (Content a) <> (Content b) = Content $ a <> b

instance Monoid Content where
  mempty = Content mempty

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)

-- * Head

instance Semigroup Head where
  (<>) (Head h1) (Head h2) =
    Head (h1 <> h2)

instance Monoid Head where
  mempty = Head mempty 

title_ :: String -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ path =
  Head $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

meta_ :: String -> String -> Head
meta_ name content =
  Head $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

getStructureString :: Structure -> String
getStructureString (Structure str) = str

getContentString :: Content -> String
getContentString (Content str) = str

render :: Html -> String
render (Html str) = str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<'  -> "&lt;"
        '>'  -> "&gt;"
        '&'  -> "&amp;"
        '"'  -> "&quot;"
        '\'' -> "&#39;"
        _    -> [c]
  in
    concatMap escapeChar

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"
