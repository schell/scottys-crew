{-# LANGUAGE OverloadedStrings #-}
module Web.Crew.User.Services where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt
import           Web.Scotty
import           Network.HTTP.Types.Status
import           Data.Maybe
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Time.Clock
import           Data.Aeson hiding (json)
import           Web.Crew.User.DB
import           Web.Crew.User.Types
import           Web.Crew.Session
import           Web.Crew.Templates
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.ByteString.Char8 as B


userRoutes :: AcidState Users -> ScottyM ()
userRoutes acid = do
    getIndex
    getLogin
    getLogout
    postLogin acid
    getSession
    getHome
    getUser acid


getIndex :: ScottyM ()
getIndex =
    get "/" $ blazePretty $ wrapper "Home" "Stuff"


getLogin :: ScottyM ()
getLogin = get "/login" $ blazePretty $ wrapper "Login" loginForm


getLogout :: ScottyM ()
getLogout = get "/logout" $ do
    deauthorize
    redirect "/"


postLogin :: AcidState (EventState PeekUserWithEmail) -> ScottyM ()
postLogin acid =
    post "/login" $ do
        email <- param "email"
        pass  <- param "password"
        dest  <- param "redirect" `rescue` (const $ return "/home") :: ActionM T.Text

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
        redirect dest


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
getUser acid = get "/account" $ ifAuthorizedPeekUser acid $ \user -> do
    json $ object [ "user" .= user
                  ]

addUserWithNameEmailPassword :: AcidState (EventState AddUser) -> TS.Text -> TS.Text -> B.ByteString -> ActionM ()
addUserWithNameEmailPassword acid name email pass = do
    Just salt <- liftIO $ genSaltUsingPolicy slowerBcryptHashingPolicy
    Just hpass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy pass
    update' acid $ AddUser name email hpass salt

