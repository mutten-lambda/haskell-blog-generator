el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_, body_, head_, title_, p_, h1_ :: String -> String
html_ = el "html"
body_ = el "body"
head_ = el "head"
title_ = el "title"
p_ = el "p"
h1_ = el "h1"

wrapHtml :: String -> String
wrapHtml = html_ . body_

makeHtml :: String -> String -> String
makeHtml title body = html_ $ title' <> body'
    where
        title' = head_ . title_ $ title
        body' = body_ body

myHtml :: String
myHtml = makeHtml "Hello title" $ h1_ "Hello, world!" <> p_ "This is a paragraph."

main :: IO ()
main = putStrLn myHtml
