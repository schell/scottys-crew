{-# LANGUAGE OverloadedStrings #-}
module Web.Crew.User.Services where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Web.Scotty
import           Network.HTTP.Types.Status
import           Data.Maybe
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Time.Clock
import           Data.Aeson hiding (json)
import qualified Data.Text.Lazy as T
import           Web.Crew.User.DB
import           Web.Crew.User.Types
import           Web.Crew.Session.Utils
import           Web.Crew.Templates


userRoutes :: AcidState Users -> ScottyM ()
userRoutes acid = do
    getIndex
    getLogin
    postLogin acid
    getSession
    getHome
    getUser acid


getIndex :: ScottyM ()
getIndex =
    get "/" $ blazePretty $ wrapper "Home" "Stuff"


getLogin :: ScottyM ()
getLogin =
    get "/login" $ blazePretty login


postLogin :: AcidState (EventState PeekUserWithEmail) -> ScottyM ()
postLogin acid =
    post "/login" $ do
        email <- param "email"
        pass  <- param "password"

        regdUser <- query' acid $ PeekUserWithEmail email
        when (isNothing regdUser) $ redirect "/login"

        -- Compare the user's password and stuff.
        let u = fromJust regdUser
        unless (userWithPasswordIsValid u pass) $
            redirect "/login"

        -- Set our cookie now that the user is authentic.
        -- writeUserCookie will take care of the expiry for us.
        writeUserCookie $ UserCookie (_userId u) zeroDay
        -- Update the last user login.
        t <- liftIO getCurrentTime
        update' acid $ UpdateUser $ u{_userLastLogin=t}
        redirect "/home"


getSession :: ScottyM ()
getSession =
    get "/session" $ authorizeAdmin $ do
        mCookie <- readUserCookie
        unless (isNothing mCookie) $ do
            let c = fromJust mCookie
            dt <- cookieExpiresIn c
            json $ object [ "cookie"     .= c
                          , "expiresIn"  .= (realToFrac dt :: Double)
                          ]


getHome :: ScottyM ()
getHome =
    get "/home" $ authorize $
        json $ object ["status" .= ("ok"::T.Text)]


ifAuthorizedPeekUser :: AcidState Users -> (User -> ActionM ()) -> ActionM ()
ifAuthorizedPeekUser acid f =
    authorizeAndId $ \uid -> do
        mUser  <- query' acid $ PeekUserWithId uid
        maybe (jsonErr forbidden403) f mUser


getUser :: AcidState (EventState PeekUserWithId) -> ScottyM ()
getUser acid =
    get "/account" $ ifAuthorizedPeekUser acid $ \user -> do
        json $ object [ "user" .= user
                      ]

