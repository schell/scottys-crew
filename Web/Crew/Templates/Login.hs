{-# LANGUAGE OverloadedStrings #-}
module Web.Crew.Templates.Login where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Web.Crew.Templates.Wrapper

login :: Html
login = wrapper "Login" loginForm



loginForm :: Html
loginForm = do
    H.form ! method "POST" $ do
        H.div ! class_ "form-group" $ do
            H.label "Email address:" ! A.for "login-email"
            input ! name "email" ! type_ "email" ! class_ "form-control" ! A.id "login-email" ! placeholder "Your email"
        H.div ! class_ "form-group" $ do
            H.label "Password:" ! A.for "login-password"
            input ! name "password" ! type_ "password" ! class_ "form-control" ! A.id "login-email" ! placeholder "Password"
        H.button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

