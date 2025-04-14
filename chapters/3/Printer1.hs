html_ content = "<html>" <> content <> "</html>"
body_ content = "<body>" <> content <> "</body>"
head_ content = "<head>" <> content <> "</head>"
title_ content = "<title>" <> content <> "</title>"

wrapHtml = html_ . body_

makeHtml title body = html_ $ title' <> body'
    where
        title' = head_ . title_ $ title
        body' = body_ body

myHtml = makeHtml "Hello title" "Hello, world!"

main = putStrLn myHtml
