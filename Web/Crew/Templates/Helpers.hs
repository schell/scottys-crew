{-# LANGUAGE OverloadedStrings #-}
module Web.Crew.Templates.Helpers where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

headWithTitle :: H.Markup -> Html
headWithTitle t = H.head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
    meta ! name "description" ! content ""
    meta ! name "author" ! content "Schell Scivally"
    H.title t
    --  Bootstrap core CSS
    link ! href "http://netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" ! rel "stylesheet"
    link ! href "http://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css" ! rel "stylesheet"
    --  Add custom CSS here
    H.style "body {margin-top: 60px;}"


