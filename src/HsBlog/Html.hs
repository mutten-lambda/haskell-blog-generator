module HsBlog.Html
  ( -- * HTML EDSL
    Html
  , html_

    -- ** Combinators used to construct the @\<head\>@ section
  , Title
  , Head
  , title_ 
  , stylesheet_
  , meta_

  -- ** Combinators used to construct the @\<body\>@ section
  , Structure
  , p_
  , h_
  , ul_
  , ol_
  , pre_
  
  -- ** Combinators used to construct content inside structures
  , Content
  , link_
  , txt_
  , img_
  , b_
  , i_

  -- ** Render HTML to String
  , render
  )
  where

import HsBlog.Html.Internal
