{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Web.Crew.Templates.Forms where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Maybe
import Control.Monad
import Data.Text as T


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


data InputData = ValidInputData T.Text
               | InvalidInputData T.Text T.Text


isValid :: InputData -> Bool
isValid (ValidInputData _) = True
isValid _ = False


inputValue :: InputData -> T.Text
inputValue (ValidInputData v) = v
inputValue (InvalidInputData v _) = v


errorValue :: InputData -> T.Text
errorValue (InvalidInputData _ v) = v
errorValue _ = ""


alwaysValid :: T.Text -> InputData
alwaysValid = ValidInputData


validIf :: T.Text -> (T.Text -> Bool, T.Text) -> InputData
validIf t (f, msg) = if f t then ValidInputData t else InvalidInputData t msg


data UserFormData = UserFormData { _ufdName      :: InputData
                                 , _ufdEmail     :: InputData
                                 , _ufdPassword  :: InputData
                                 , _ufdPassCheck :: InputData
                                 }


emptyUserFormData :: UserFormData
emptyUserFormData = UserFormData { _ufdName = ValidInputData ""
                                 , _ufdEmail = ValidInputData ""
                                 , _ufdPassword = ValidInputData ""
                                 , _ufdPassCheck = ValidInputData ""
                                 }


validateUserFormData :: T.Text -> T.Text -> T.Text -> T.Text -> UserFormData
validateUserFormData name' email pass passcheck =
    UserFormData (alwaysValid name')
                 (alwaysValid email)
                 (alwaysValid pass)
                 (validIf passcheck ((== pass), "Passwords do not match."))


userFormDataIsValid :: UserFormData -> Bool
userFormDataIsValid (UserFormData name' email pass passcheck) =
    and $ Prelude.map isValid [name', email, pass, passcheck]


addUserForm :: Maybe UserFormData -> Html
addUserForm mUFD = do
    let UserFormData{..} = fromMaybe emptyUserFormData mUFD
        nameClass = toValue $ ("form-group" ++ if isValid _ufdName then "" else " has-error" :: String)
        passClass = toValue $ ("form-group" ++ if isValid _ufdPassCheck then "" else " has-error" :: String)

    H.form ! method "POST" $ do
        H.div ! class_ nameClass $ do
            H.label "user name:" ! A.for "add-user-name"
            input !  name "add-user-name" ! type_ "text" ! class_ "form-control"
                  ! A.id "add-user-name" ! placeholder "bigcheese77"
                  ! value (toValue $ inputValue _ufdName)
            when (not $ isValid _ufdName) $ H.span ! class_ "help-block" $ toHtml $ errorValue _ufdName

        H.div ! class_ "form-group" $ do
            H.label "user email:" ! A.for "add-user-name"
            input ! name "add-user-email" ! type_ "text" ! class_ "form-control" ! A.id "add-user-email" ! placeholder "big.cheese@superpharma.net"
        H.div ! class_ passClass $ do
            H.label "user password:" ! A.for "add-user-pass"
            H.input ! name "add-user-pass" ! type_ "password"
                    ! class_ "form-control" ! A.id "add-user-pass"

            H.label "confirm password:" ! A.for "add-user-passcheck"
            H.input ! name "add-user-passcheck" ! type_ "password"
                    ! class_ "form-control" ! A.id "add-user-passcheck"
            when (not $ isValid _ufdPassCheck) $ H.span ! class_ "help-block" $ toHtml $ errorValue _ufdPassCheck
        H.button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

