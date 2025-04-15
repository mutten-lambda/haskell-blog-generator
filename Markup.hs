module Markup 
  ( Document
  , Structure (..)
  )
  where

import Numeric.Natural ( Natural )

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | Codeblock [String]

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts = 
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in 
    case txts of
      [] -> [paragraph]
      currentline : rest -> 
        if trim currentline == ""
        then paragraph : parseLines [] rest
        else parseLines (currentline : currentParagraph) rest

trim :: String -> String
trim = unwords . words
