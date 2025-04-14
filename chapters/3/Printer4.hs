newtype Html = Html String
newtype Structure = Structure String
type Title = String

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_, body_, head_, title_, p_, h1_ :: String -> Structure
html_ = Structure . el "html"
body_ = Structure . el "body"
head_ = Structure . el "head"
title_ = Structure . el "title"
p_ = Structure . el "p"
h1_ = Structure . el "h1"

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

render :: Html -> String
render (Html a) = a

makeHtml :: Title -> Structure -> Html
makeHtml title (Structure body) =
    Html . el "html" $ title' <> body'
        where
            title' = el "head" . el "title" $ title
            body' = el "body" body

myHtml :: Html
myHtml = makeHtml "Hello title" $ h1_ "Hello, world!" `append_` p_ "This is a paragraph."

main :: IO ()
main = putStrLn . render $ myHtml
