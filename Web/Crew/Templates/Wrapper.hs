{-# LANGUAGE OverloadedStrings #-}
module Web.Crew.Templates.Wrapper where

import Prelude
import Data.Monoid (mempty)

import Web.Crew.Templates.Helpers
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

wrapper :: H.Markup -> H.Markup -> Html
wrapper t content =
    docTypeHtml ! A.lang "en" $ do
        headWithTitle t
        body $ do
            nav ! A.class_ "navbar navbar-inverse navbar-fixed-top" $
                H.div ! A.class_ "container" $ do
                    H.div ! A.class_ "navbar-header" $ do
                        button ! A.type_ "button" ! A.class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-ex1-collapse" $ do
                            H.span ! A.class_ "sr-only" $ "Toggle navigation"
                            H.span ! A.class_ "icon-bar" $ mempty
                            H.span ! A.class_ "icon-bar" $ mempty
                            H.span ! A.class_ "icon-bar" $ mempty
                        a ! A.class_ "navbar-brand" ! A.href "/" $ "Cointree.us"
                    --  Collect the nav links, forms, and other content for toggling
                    H.div ! A.class_ "collapse navbar-collapse navbar-ex1-collapse" $ ul ! A.class_ "nav navbar-nav" $ do
                        li $ a ! A.href "#about" $ "About"
                        li $ a ! A.href "#services" $ "Services"
                        li $ a ! A.href "#contact" $ "Contact"
                    --  /.navbar-collapse
                --  /.container
            H.div ! A.class_ "container" $ H.div ! A.class_ "row" $ H.div ! A.class_ "col-lg-12" $ content
            --  /.container
            --  JavaScript
            script ! A.src "http://code.jquery.com/jquery-1.10.2.min.js" $ mempty
            script ! A.src "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js" $ mempty
