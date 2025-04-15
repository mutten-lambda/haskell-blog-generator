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
